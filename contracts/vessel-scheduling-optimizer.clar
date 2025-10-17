;; Vessel Scheduling Optimizer Contract
;; Manages vessel scheduling, berth allocation, and port capacity optimization

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u100))
(define-constant ERR-NOT-FOUND (err u101))
(define-constant ERR-INVALID-PARAMS (err u102))
(define-constant ERR-UNAUTHORIZED (err u103))
(define-constant ERR-BERTH-OCCUPIED (err u104))
(define-constant ERR-VESSEL-TOO-LARGE (err u105))
(define-constant ERR-SCHEDULING-CONFLICT (err u106))

;; Data Variables
(define-data-var next-vessel-id uint u1)
(define-data-var next-berth-id uint u1)
(define-data-var next-schedule-id uint u1)
(define-data-var port-operational bool true)

;; Data Maps
;; Vessel registration and specifications
(define-map vessels
  uint ;; vessel-id
  {
    name: (string-ascii 64),
    imo-number: (string-ascii 16),
    owner-address: principal,
    vessel-type: (string-ascii 32),
    length: uint,
    beam: uint,
    draft: uint,
    gross-tonnage: uint,
    cargo-capacity: uint,
    flag-state: (string-ascii 32),
    is-active: bool,
    registered-at: uint
  }
)

;; Berth information and specifications
(define-map berths
  uint ;; berth-id
  {
    berth-name: (string-ascii 32),
    max-vessel-length: uint,
    max-vessel-beam: uint,
    max-draft: uint,
    berth-type: (string-ascii 32),
    equipment-available: (list 10 (string-ascii 32)),
    hourly-rate: uint,
    is-operational: bool,
    water-depth: uint,
    crane-capacity: uint
  }
)

;; Vessel scheduling and berth assignments
(define-map vessel-schedules
  uint ;; schedule-id
  {
    vessel-id: uint,
    berth-id: uint,
    arrival-time: uint,
    departure-time: uint,
    estimated-duration: uint,
    actual-arrival: uint,
    actual-departure: uint,
    schedule-status: (string-ascii 16),
    priority-level: uint,
    created-at: uint
  }
)

;; Port capacity and utilization tracking
(define-map port-capacity
  uint ;; time-slot (block height)
  {
    total-berths: uint,
    occupied-berths: uint,
    available-berths: uint,
    utilization-rate: uint,
    vessels-waiting: uint,
    average-wait-time: uint
  }
)

;; Vessel arrival notifications
(define-map arrival-notifications
  { vessel-id: uint, notification-time: uint }
  {
    estimated-arrival: uint,
    cargo-manifest: (string-ascii 128),
    special-requirements: (string-ascii 64),
    customs-status: (string-ascii 16),
    pilot-required: bool,
    tugboat-required: bool
  }
)

;; Performance metrics
(define-map performance-metrics
  uint ;; period (daily/weekly)
  {
    total-vessels-served: uint,
    average-turnaround-time: uint,
    average-wait-time: uint,
    berth-utilization: uint,
    revenue-generated: uint,
    efficiency-score: uint,
    customer-satisfaction: uint
  }
)

;; Wait time optimization
(define-map wait-time-optimization
  uint ;; vessel-id
  {
    predicted-wait-time: uint,
    actual-wait-time: uint,
    delay-factors: (list 5 (string-ascii 32)),
    optimization-suggestions: (string-ascii 128),
    cost-impact: uint
  }
)

;; Authorization
(define-map authorized-operators principal bool)
(define-map port-authorities principal bool)

;; Private Functions
(define-private (is-authorized (caller principal))
  (or 
    (is-eq caller CONTRACT-OWNER)
    (default-to false (map-get? authorized-operators caller))
    (default-to false (map-get? port-authorities caller))
  )
)

(define-private (get-next-vessel-id)
  (let ((current-id (var-get next-vessel-id)))
    (var-set next-vessel-id (+ current-id u1))
    current-id
  )
)

(define-private (get-next-berth-id)
  (let ((current-id (var-get next-berth-id)))
    (var-set next-berth-id (+ current-id u1))
    current-id
  )
)

(define-private (get-next-schedule-id)
  (let ((current-id (var-get next-schedule-id)))
    (var-set next-schedule-id (+ current-id u1))
    current-id
  )
)

(define-private (vessel-fits-berth (vessel-length uint) (vessel-beam uint) (vessel-draft uint) (berth-id uint))
  (match (map-get? berths berth-id)
    berth-data
    (and 
      (<= vessel-length (get max-vessel-length berth-data))
      (<= vessel-beam (get max-vessel-beam berth-data))
      (<= vessel-draft (get max-draft berth-data))
    )
    false
  )
)

(define-private (calculate-turnaround-time (cargo-capacity uint) (cargo-type (string-ascii 32)))
  (if (is-eq cargo-type "container")
    (/ cargo-capacity u50)  ;; 50 containers per hour
    (if (is-eq cargo-type "bulk")
      (/ cargo-capacity u200)  ;; 200 tons per hour
      (/ cargo-capacity u100)  ;; default rate
    )
  )
)

(define-private (is-berth-available (berth-id uint) (start-time uint) (end-time uint))
  ;; Simplified availability check - in practice would check all existing schedules
  (get is-operational (default-to 
    { berth-name: "", max-vessel-length: u0, max-vessel-beam: u0, max-draft: u0, 
      berth-type: "", equipment-available: (list), hourly-rate: u0, 
      is-operational: false, water-depth: u0, crane-capacity: u0 }
    (map-get? berths berth-id)
  ))
)

;; Public Functions

;; Register new vessel
(define-public (register-vessel
  (name (string-ascii 64))
  (imo-number (string-ascii 16))
  (vessel-type (string-ascii 32))
  (length uint)
  (beam uint)
  (draft uint)
  (gross-tonnage uint)
  (cargo-capacity uint)
  (flag-state (string-ascii 32))
)
  (let ((vessel-id (get-next-vessel-id)))
    (asserts! (and (> length u0) (> beam u0) (> draft u0)) ERR-INVALID-PARAMS)
    
    (map-set vessels vessel-id {
      name: name,
      imo-number: imo-number,
      owner-address: tx-sender,
      vessel-type: vessel-type,
      length: length,
      beam: beam,
      draft: draft,
      gross-tonnage: gross-tonnage,
      cargo-capacity: cargo-capacity,
      flag-state: flag-state,
      is-active: true,
      registered-at: burn-block-height
    })
    
    (ok vessel-id)
  )
)

;; Register new berth
(define-public (register-berth
  (berth-name (string-ascii 32))
  (max-vessel-length uint)
  (max-vessel-beam uint)
  (max-draft uint)
  (berth-type (string-ascii 32))
  (hourly-rate uint)
  (water-depth uint)
  (crane-capacity uint)
)
  (let ((berth-id (get-next-berth-id)))
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (and (> max-vessel-length u0) (> water-depth u0)) ERR-INVALID-PARAMS)
    
    (map-set berths berth-id {
      berth-name: berth-name,
      max-vessel-length: max-vessel-length,
      max-vessel-beam: max-vessel-beam,
      max-draft: max-draft,
      berth-type: berth-type,
      equipment-available: (list "crane" "container-handling"),
      hourly-rate: hourly-rate,
      is-operational: true,
      water-depth: water-depth,
      crane-capacity: crane-capacity
    })
    
    (ok berth-id)
  )
)

;; Schedule vessel arrival
(define-public (schedule-vessel-arrival
  (vessel-id uint)
  (berth-id uint)
  (arrival-time uint)
  (estimated-duration uint)
  (priority-level uint)
)
  (let 
    (
      (schedule-id (get-next-schedule-id))
      (vessel-data (map-get? vessels vessel-id))
      (berth-data (map-get? berths berth-id))
      (departure-time (+ arrival-time estimated-duration))
    )
    (asserts! (is-some vessel-data) ERR-NOT-FOUND)
    (asserts! (is-some berth-data) ERR-NOT-FOUND)
    (asserts! (> arrival-time burn-block-height) ERR-INVALID-PARAMS)
    
    (let 
      (
        (vessel (unwrap-panic vessel-data))
        (berth (unwrap-panic berth-data))
      )
      (asserts! (vessel-fits-berth 
        (get length vessel) 
        (get beam vessel) 
        (get draft vessel) 
        berth-id
      ) ERR-VESSEL-TOO-LARGE)
      
      (asserts! (is-berth-available berth-id arrival-time departure-time) ERR-BERTH-OCCUPIED)
      
      (map-set vessel-schedules schedule-id {
        vessel-id: vessel-id,
        berth-id: berth-id,
        arrival-time: arrival-time,
        departure-time: departure-time,
        estimated-duration: estimated-duration,
        actual-arrival: u0,
        actual-departure: u0,
        schedule-status: "scheduled",
        priority-level: priority-level,
        created-at: burn-block-height
      })
      
      (ok schedule-id)
    )
  )
)

;; Update vessel arrival (actual arrival)
(define-public (update-vessel-arrival (schedule-id uint) (actual-arrival-time uint))
  (let ((schedule (map-get? vessel-schedules schedule-id)))
    (asserts! (is-some schedule) ERR-NOT-FOUND)
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    
    (let ((schedule-data (unwrap-panic schedule)))
      (map-set vessel-schedules schedule-id
        (merge schedule-data {
          actual-arrival: actual-arrival-time,
          schedule-status: "arrived"
        })
      )
      
      (ok true)
    )
  )
)

;; Process vessel departure
(define-public (process-vessel-departure (schedule-id uint) (actual-departure-time uint))
  (let ((schedule (map-get? vessel-schedules schedule-id)))
    (asserts! (is-some schedule) ERR-NOT-FOUND)
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    
    (let ((schedule-data (unwrap-panic schedule)))
      (asserts! (is-eq (get schedule-status schedule-data) "arrived") ERR-INVALID-PARAMS)
      
      (map-set vessel-schedules schedule-id
        (merge schedule-data {
          actual-departure: actual-departure-time,
          schedule-status: "completed"
        })
      )
      
      (ok true)
    )
  )
)

;; Submit arrival notification
(define-public (submit-arrival-notification
  (vessel-id uint)
  (estimated-arrival uint)
  (cargo-manifest (string-ascii 128))
  (special-requirements (string-ascii 64))
  (pilot-required bool)
)
  (begin
    (asserts! (is-some (map-get? vessels vessel-id)) ERR-NOT-FOUND)
    (asserts! (> estimated-arrival burn-block-height) ERR-INVALID-PARAMS)
    
    (map-set arrival-notifications
      { vessel-id: vessel-id, notification-time: burn-block-height }
      {
        estimated-arrival: estimated-arrival,
        cargo-manifest: cargo-manifest,
        special-requirements: special-requirements,
        customs-status: "pending",
        pilot-required: pilot-required,
        tugboat-required: true
      }
    )
    
    (ok true)
  )
)

;; Update port capacity metrics
(define-public (update-port-capacity
  (total-berths uint)
  (occupied-berths uint)
  (vessels-waiting uint)
  (average-wait-time uint)
)
  (begin
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (>= total-berths occupied-berths) ERR-INVALID-PARAMS)
    
    (let 
      (
        (available-berths (- total-berths occupied-berths))
        (utilization-rate (/ (* occupied-berths u100) total-berths))
      )
      (map-set port-capacity burn-block-height {
        total-berths: total-berths,
        occupied-berths: occupied-berths,
        available-berths: available-berths,
        utilization-rate: utilization-rate,
        vessels-waiting: vessels-waiting,
        average-wait-time: average-wait-time
      })
      
      (ok true)
    )
  )
)

;; Optimize vessel scheduling
(define-public (optimize-vessel-wait-time
  (vessel-id uint)
  (predicted-wait uint)
  (delay-factors (list 5 (string-ascii 32)))
  (optimization-suggestions (string-ascii 128))
)
  (begin
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? vessels vessel-id)) ERR-NOT-FOUND)
    
    (map-set wait-time-optimization vessel-id {
      predicted-wait-time: predicted-wait,
      actual-wait-time: u0,
      delay-factors: delay-factors,
      optimization-suggestions: optimization-suggestions,
      cost-impact: (* predicted-wait u50)  ;; $50 per hour wait cost
    })
    
    (ok true)
  )
)

;; Authorize port operator
(define-public (authorize-operator (operator principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-operators operator true)
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-vessel (vessel-id uint))
  (map-get? vessels vessel-id)
)

(define-read-only (get-berth (berth-id uint))
  (map-get? berths berth-id)
)

(define-read-only (get-vessel-schedule (schedule-id uint))
  (map-get? vessel-schedules schedule-id)
)

(define-read-only (get-port-capacity (time-slot uint))
  (map-get? port-capacity time-slot)
)

(define-read-only (get-arrival-notification (vessel-id uint) (notification-time uint))
  (map-get? arrival-notifications { vessel-id: vessel-id, notification-time: notification-time })
)

(define-read-only (check-berth-compatibility (vessel-id uint) (berth-id uint))
  (match (map-get? vessels vessel-id)
    vessel-data
    (ok (vessel-fits-berth 
      (get length vessel-data) 
      (get beam vessel-data) 
      (get draft vessel-data) 
      berth-id
    ))
    ERR-NOT-FOUND
  )
)

(define-read-only (estimate-turnaround-time (vessel-id uint))
  (match (map-get? vessels vessel-id)
    vessel-data
    (ok (calculate-turnaround-time 
      (get cargo-capacity vessel-data) 
      (get vessel-type vessel-data)
    ))
    ERR-NOT-FOUND
  )
)


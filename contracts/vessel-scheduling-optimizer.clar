;; Vessel Scheduling Optimizer
;; Smart contract for optimizing vessel scheduling and berth allocation at ports
;; Manages vessel queues, berth assignments, and port capacity optimization

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-already-exists (err u102))
(define-constant err-invalid-berth (err u103))
(define-constant err-berth-occupied (err u104))
(define-constant err-insufficient-capacity (err u105))
(define-constant err-invalid-vessel (err u106))
(define-constant err-scheduling-conflict (err u107))
(define-constant err-unauthorized-access (err u108))

;; Data Variables
(define-data-var next-vessel-id uint u1)
(define-data-var next-berth-id uint u1)
(define-data-var total-port-capacity uint u100)
(define-data-var current-utilization uint u0)
(define-data-var port-operator principal tx-sender)

;; Data Maps
(define-map vessels 
    { vessel-id: uint } 
    {
        vessel-name: (string-ascii 50),
        vessel-type: (string-ascii 20),
        length: uint,
        capacity: uint,
        owner: principal,
        arrival-time: uint,
        departure-time: (optional uint),
        status: (string-ascii 20),
        assigned-berth: (optional uint),
        priority-level: uint,
        created-at: uint
    }
)

(define-map berths
    { berth-id: uint }
    {
        berth-name: (string-ascii 30),
        capacity: uint,
        vessel-type-supported: (string-ascii 20),
        occupied: bool,
        current-vessel: (optional uint),
        location: (string-ascii 50),
        maintenance-status: (string-ascii 20),
        created-at: uint
    }
)

(define-map vessel-queue
    { position: uint }
    {
        vessel-id: uint,
        estimated-wait-time: uint,
        queue-timestamp: uint
    }
)

(define-map berth-schedule
    { berth-id: uint, time-slot: uint }
    {
        vessel-id: uint,
        scheduled-arrival: uint,
        scheduled-departure: uint,
        confirmed: bool
    }
)

(define-map port-statistics
    { stat-type: (string-ascii 30) }
    {
        value: uint,
        last-updated: uint,
        operator: principal
    }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-port-operator)
    (is-eq tx-sender (var-get port-operator))
)

(define-private (calculate-vessel-priority (vessel-type (string-ascii 20)) (arrival-time uint))
    (if (is-eq vessel-type "emergency")
        u10
        (if (is-eq vessel-type "cargo")
            u5
            (if (< arrival-time (+ burn-block-height u144))
                u7
                u3
            )
        )
    )
)

(define-private (find-suitable-berth (vessel-id uint))
    (let ((vessel-data (unwrap! (map-get? vessels { vessel-id: vessel-id }) (err u404))))
        (let ((vessel-type (get vessel-type vessel-data))
              (vessel-length (get length vessel-data)))
            (match (map-get? berths { berth-id: u1 })
                berth-data
                    (if (and 
                        (not (get occupied berth-data))
                        (is-eq (get vessel-type-supported berth-data) vessel-type)
                        (>= (get capacity berth-data) vessel-length)
                        (is-eq (get maintenance-status berth-data) "operational"))
                        (ok u1)
                        (err u404)
                    )
                (err u404)
            )
        )
    )
)

(define-private (update-port-utilization)
    (let ((current-vessels (var-get current-utilization))
          (total-capacity (var-get total-port-capacity)))
        (var-set current-utilization 
            (/ (* current-vessels u100) total-capacity)
        )
    )
)

(define-private (calculate-wait-time (vessel-priority uint))
    (let ((base-wait-time u60)
          (priority-multiplier (if (> vessel-priority u7) u1 u2)))
        (* base-wait-time priority-multiplier)
    )
)

;; Public Functions
(define-public (register-vessel 
    (vessel-name (string-ascii 50))
    (vessel-type (string-ascii 20))
    (length uint)
    (capacity uint)
    (arrival-time uint)
    )
    (let ((vessel-id (var-get next-vessel-id))
          (priority (calculate-vessel-priority vessel-type arrival-time)))
        (asserts! (> length u0) err-invalid-vessel)
        (asserts! (> capacity u0) err-invalid-vessel)
        (asserts! (>= arrival-time burn-block-height) err-invalid-vessel)
        
        (map-set vessels
            { vessel-id: vessel-id }
            {
                vessel-name: vessel-name,
                vessel-type: vessel-type,
                length: length,
                capacity: capacity,
                owner: tx-sender,
                arrival-time: arrival-time,
                departure-time: none,
                status: "registered",
                assigned-berth: none,
                priority-level: priority,
                created-at: burn-block-height
            }
        )
        
        (var-set next-vessel-id (+ vessel-id u1))
        (ok vessel-id)
    )
)

(define-public (create-berth
    (berth-name (string-ascii 30))
    (capacity uint)
    (vessel-type-supported (string-ascii 20))
    (location (string-ascii 50))
    )
    (let ((berth-id (var-get next-berth-id)))
        (asserts! (is-port-operator) err-owner-only)
        (asserts! (> capacity u0) err-invalid-berth)
        
        (map-set berths
            { berth-id: berth-id }
            {
                berth-name: berth-name,
                capacity: capacity,
                vessel-type-supported: vessel-type-supported,
                occupied: false,
                current-vessel: none,
                location: location,
                maintenance-status: "operational",
                created-at: burn-block-height
            }
        )
        
        (var-set next-berth-id (+ berth-id u1))
        (var-set total-port-capacity (+ (var-get total-port-capacity) capacity))
        (ok berth-id)
    )
)

(define-public (assign-berth (vessel-id uint) (berth-id uint))
    (let ((vessel-data (unwrap! (map-get? vessels { vessel-id: vessel-id }) err-not-found))
          (berth-data (unwrap! (map-get? berths { berth-id: berth-id }) err-not-found)))
        (asserts! (is-port-operator) err-unauthorized-access)
        (asserts! (not (get occupied berth-data)) err-berth-occupied)
        (asserts! (is-none (get assigned-berth vessel-data)) err-already-exists)
        
        ;; Update vessel with berth assignment
        (map-set vessels
            { vessel-id: vessel-id }
            (merge vessel-data {
                assigned-berth: (some berth-id),
                status: "docked"
            })
        )
        
        ;; Update berth as occupied
        (map-set berths
            { berth-id: berth-id }
            (merge berth-data {
                occupied: true,
                current-vessel: (some vessel-id)
            })
        )
        
        (var-set current-utilization (+ (var-get current-utilization) u1))
        (update-port-utilization)
        (ok true)
    )
)

(define-public (schedule-departure (vessel-id uint) (departure-time uint))
    (let ((vessel-data (unwrap! (map-get? vessels { vessel-id: vessel-id }) err-not-found)))
        (asserts! (or (is-eq tx-sender (get owner vessel-data)) (is-port-operator)) err-unauthorized-access)
        (asserts! (> departure-time burn-block-height) err-scheduling-conflict)
        (asserts! (is-eq (get status vessel-data) "docked") err-invalid-vessel)
        
        (map-set vessels
            { vessel-id: vessel-id }
            (merge vessel-data {
                departure-time: (some departure-time),
                status: "scheduled-departure"
            })
        )
        
        (ok true)
    )
)

(define-public (release-berth (vessel-id uint))
    (let ((vessel-data (unwrap! (map-get? vessels { vessel-id: vessel-id }) err-not-found))
          (berth-id (unwrap! (get assigned-berth vessel-data) err-not-found))
          (berth-data (unwrap! (map-get? berths { berth-id: berth-id }) err-not-found)))
        (asserts! (or (is-eq tx-sender (get owner vessel-data)) (is-port-operator)) err-unauthorized-access)
        
        ;; Update vessel status
        (map-set vessels
            { vessel-id: vessel-id }
            (merge vessel-data {
                status: "departed",
                assigned-berth: none
            })
        )
        
        ;; Release berth
        (map-set berths
            { berth-id: berth-id }
            (merge berth-data {
                occupied: false,
                current-vessel: none
            })
        )
        
        (var-set current-utilization (- (var-get current-utilization) u1))
        (update-port-utilization)
        (ok true)
    )
)

(define-public (add-to-queue (vessel-id uint))
    (let ((vessel-data (unwrap! (map-get? vessels { vessel-id: vessel-id }) err-not-found))
          (queue-position (var-get current-utilization))
          (priority (get priority-level vessel-data))
          (wait-time (calculate-wait-time priority)))
        (asserts! (is-eq (get status vessel-data) "registered") err-invalid-vessel)
        
        (map-set vessel-queue
            { position: queue-position }
            {
                vessel-id: vessel-id,
                estimated-wait-time: wait-time,
                queue-timestamp: burn-block-height
            }
        )
        
        (map-set vessels
            { vessel-id: vessel-id }
            (merge vessel-data { status: "queued" })
        )
        
        (ok queue-position)
    )
)

(define-public (update-port-statistics (stat-type (string-ascii 30)) (value uint))
    (begin
        (asserts! (is-port-operator) err-unauthorized-access)
        (map-set port-statistics
            { stat-type: stat-type }
            {
                value: value,
                last-updated: burn-block-height,
                operator: tx-sender
            }
        )
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-vessel (vessel-id uint))
    (map-get? vessels { vessel-id: vessel-id })
)

(define-read-only (get-berth (berth-id uint))
    (map-get? berths { berth-id: berth-id })
)

(define-read-only (get-port-capacity)
    {
        total-capacity: (var-get total-port-capacity),
        current-utilization: (var-get current-utilization),
        available-capacity: (- (var-get total-port-capacity) (var-get current-utilization))
    }
)

(define-read-only (get-vessel-queue-status (position uint))
    (map-get? vessel-queue { position: position })
)

(define-read-only (get-port-statistics-data (stat-type (string-ascii 30)))
    (map-get? port-statistics { stat-type: stat-type })
)

(define-read-only (get-contract-info)
    {
        owner: contract-owner,
        port-operator: (var-get port-operator),
        next-vessel-id: (var-get next-vessel-id),
        next-berth-id: (var-get next-berth-id),
        total-capacity: (var-get total-port-capacity)
    }
)


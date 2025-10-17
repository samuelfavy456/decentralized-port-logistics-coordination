;; Cargo Handling Coordinator Contract
;; Manages cargo operations, container movements, and inland transportation coordination

;; Constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-OWNER-ONLY (err u200))
(define-constant ERR-NOT-FOUND (err u201))
(define-constant ERR-INVALID-PARAMS (err u202))
(define-constant ERR-UNAUTHORIZED (err u203))
(define-constant ERR-CARGO-DAMAGED (err u204))
(define-constant ERR-EQUIPMENT-BUSY (err u205))
(define-constant ERR-CUSTOMS-PENDING (err u206))

;; Data Variables
(define-data-var next-cargo-id uint u1)
(define-data-var next-container-id uint u1)
(define-data-var next-operation-id uint u1)
(define-data-var cargo-operations-active bool true)

;; Data Maps
;; Cargo manifests and tracking
(define-map cargo-manifests
  uint ;; cargo-id
  {
    vessel-id: uint,
    cargo-type: (string-ascii 32),
    description: (string-ascii 128),
    weight: uint,
    volume: uint,
    value: uint,
    origin-port: (string-ascii 32),
    destination-port: (string-ascii 32),
    shipper: (string-ascii 64),
    consignee: (string-ascii 64),
    cargo-status: (string-ascii 16),
    priority-level: uint,
    created-at: uint
  }
)

;; Container tracking and management
(define-map containers
  (string-ascii 16) ;; container-number
  {
    container-id: uint,
    container-type: (string-ascii 16),
    size: (string-ascii 8),
    weight: uint,
    cargo-id: uint,
    current-location: (string-ascii 64),
    container-condition: (string-ascii 16),
    seal-number: (string-ascii 16),
    temperature-controlled: bool,
    customs-cleared: bool,
    last-updated: uint
  }
)

;; Equipment allocation and scheduling
(define-map equipment-allocation
  uint ;; operation-id
  {
    equipment-type: (string-ascii 32),
    equipment-id: (string-ascii 16),
    operator-id: (string-ascii 16),
    start-time: uint,
    end-time: uint,
    location: (string-ascii 64),
    equipment-status: (string-ascii 16),
    utilization-rate: uint
  }
)

;; Loading and unloading operations
(define-map cargo-operations
  uint ;; operation-id
  {
    vessel-id: uint,
    cargo-id: uint,
    operation-type: (string-ascii 16), ;; "loading" or "unloading"
    berth-id: uint,
    crane-id: (string-ascii 16),
    start-time: uint,
    estimated-completion: uint,
    actual-completion: uint,
    containers-processed: uint,
    operation-status: (string-ascii 16),
    efficiency-score: uint
  }
)

;; Inland transportation coordination
(define-map transport-coordination
  uint ;; transport-id
  {
    cargo-id: uint,
    transport-mode: (string-ascii 16), ;; "truck", "rail", "barge"
    transport-company: (string-ascii 64),
    pickup-time: uint,
    delivery-time: uint,
    destination: (string-ascii 128),
    transport-status: (string-ascii 16),
    tracking-number: (string-ascii 32),
    cost: uint
  }
)

;; Customs clearance workflow
(define-map customs-clearance
  uint ;; cargo-id
  {
    customs-reference: (string-ascii 32),
    declaration-type: (string-ascii 16),
    submitted-documents: (list 10 (string-ascii 32)),
    clearance-status: (string-ascii 16),
    duties-paid: bool,
    inspection-required: bool,
    clearance-date: uint,
    customs-officer: (string-ascii 32)
  }
)

;; Cargo flow optimization
(define-map cargo-flow
  uint ;; flow-id
  {
    entry-gate: (string-ascii 16),
    exit-gate: (string-ascii 16),
    processing-time: uint,
    queue-length: uint,
    bottleneck-points: (list 5 (string-ascii 32)),
    optimization-score: uint,
    throughput-rate: uint
  }
)

;; Equipment performance tracking
(define-map equipment-performance
  (string-ascii 16) ;; equipment-id
  {
    equipment-type: (string-ascii 32),
    daily-utilization: uint,
    maintenance-status: (string-ascii 16),
    breakdown-incidents: uint,
    efficiency-rating: uint,
    last-maintenance: uint,
    next-maintenance: uint
  }
)

;; Authorization
(define-map authorized-handlers principal bool)
(define-map customs-officers principal bool)

;; Private Functions
(define-private (is-authorized (caller principal))
  (or 
    (is-eq caller CONTRACT-OWNER)
    (default-to false (map-get? authorized-handlers caller))
  )
)

(define-private (get-next-cargo-id)
  (let ((current-id (var-get next-cargo-id)))
    (var-set next-cargo-id (+ current-id u1))
    current-id
  )
)

(define-private (get-next-container-id)
  (let ((current-id (var-get next-container-id)))
    (var-set next-container-id (+ current-id u1))
    current-id
  )
)

(define-private (get-next-operation-id)
  (let ((current-id (var-get next-operation-id)))
    (var-set next-operation-id (+ current-id u1))
    current-id
  )
)

(define-private (calculate-processing-time (cargo-weight uint) (cargo-type (string-ascii 32)))
  (if (is-eq cargo-type "container")
    (/ cargo-weight u2000)  ;; 2000kg per hour for containers
    (if (is-eq cargo-type "bulk")
      (/ cargo-weight u5000)  ;; 5000kg per hour for bulk
      (/ cargo-weight u3000)  ;; default processing rate
    )
  )
)

(define-private (is-equipment-available (equipment-id (string-ascii 16)) (start-time uint) (end-time uint))
  ;; Simplified availability check
  (is-eq (get equipment-status (default-to 
    { equipment-type: "", equipment-id: "", operator-id: "", start-time: u0, 
      end-time: u0, location: "", equipment-status: "unavailable", utilization-rate: u0 }
    (map-get? equipment-allocation u1)
  )) "available")
)

;; Public Functions

;; Register cargo manifest
(define-public (register-cargo-manifest
  (vessel-id uint)
  (cargo-type (string-ascii 32))
  (description (string-ascii 128))
  (weight uint)
  (volume uint)
  (value uint)
  (origin-port (string-ascii 32))
  (destination-port (string-ascii 32))
  (shipper (string-ascii 64))
  (consignee (string-ascii 64))
  (priority-level uint)
)
  (let ((cargo-id (get-next-cargo-id)))
    (asserts! (and (> weight u0) (> volume u0)) ERR-INVALID-PARAMS)
    
    (map-set cargo-manifests cargo-id {
      vessel-id: vessel-id,
      cargo-type: cargo-type,
      description: description,
      weight: weight,
      volume: volume,
      value: value,
      origin-port: origin-port,
      destination-port: destination-port,
      shipper: shipper,
      consignee: consignee,
      cargo-status: "registered",
      priority-level: priority-level,
      created-at: burn-block-height
    })
    
    (ok cargo-id)
  )
)

;; Track container
(define-public (track-container
  (container-number (string-ascii 16))
  (container-type (string-ascii 16))
  (size (string-ascii 8))
  (weight uint)
  (cargo-id uint)
  (current-location (string-ascii 64))
  (seal-number (string-ascii 16))
  (temperature-controlled bool)
)
  (begin
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (> weight u0) ERR-INVALID-PARAMS)
    
    (let ((container-id (get-next-container-id)))
      (map-set containers container-number {
        container-id: container-id,
        container-type: container-type,
        size: size,
        weight: weight,
        cargo-id: cargo-id,
        current-location: current-location,
        container-condition: "good",
        seal-number: seal-number,
        temperature-controlled: temperature-controlled,
        customs-cleared: false,
        last-updated: burn-block-height
      })
      
      (ok container-id)
    )
  )
)

;; Schedule cargo operation
(define-public (schedule-cargo-operation
  (vessel-id uint)
  (cargo-id uint)
  (operation-type (string-ascii 16))
  (berth-id uint)
  (crane-id (string-ascii 16))
  (estimated-duration uint)
)
  (let ((operation-id (get-next-operation-id)))
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? cargo-manifests cargo-id)) ERR-NOT-FOUND)
    
    (let 
      (
        (start-time (+ burn-block-height u10))  ;; Schedule 10 blocks ahead
        (completion-time (+ start-time estimated-duration))
      )
      (asserts! (is-equipment-available crane-id start-time completion-time) ERR-EQUIPMENT-BUSY)
      
      (map-set cargo-operations operation-id {
        vessel-id: vessel-id,
        cargo-id: cargo-id,
        operation-type: operation-type,
        berth-id: berth-id,
        crane-id: crane-id,
        start-time: start-time,
        estimated-completion: completion-time,
        actual-completion: u0,
        containers-processed: u0,
        operation-status: "scheduled",
        efficiency-score: u0
      })
      
      (ok operation-id)
    )
  )
)

;; Update container location
(define-public (update-container-location
  (container-number (string-ascii 16))
  (new-location (string-ascii 64))
  (condition (string-ascii 16))
)
  (let ((container (map-get? containers container-number)))
    (asserts! (is-some container) ERR-NOT-FOUND)
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    
    (let ((container-data (unwrap-panic container)))
      (map-set containers container-number
        (merge container-data {
          current-location: new-location,
          container-condition: condition,
          last-updated: burn-block-height
        })
      )
      
      (ok true)
    )
  )
)

;; Coordinate inland transport
(define-public (coordinate-transport
  (cargo-id uint)
  (transport-mode (string-ascii 16))
  (transport-company (string-ascii 64))
  (pickup-time uint)
  (destination (string-ascii 128))
  (cost uint)
)
  (let ((transport-id cargo-id))  ;; Use cargo-id as transport-id for simplicity
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? cargo-manifests cargo-id)) ERR-NOT-FOUND)
    
    (map-set transport-coordination transport-id {
      cargo-id: cargo-id,
      transport-mode: transport-mode,
      transport-company: transport-company,
      pickup-time: pickup-time,
      delivery-time: (+ pickup-time u144),  ;; 24 hours for delivery
      destination: destination,
      transport-status: "scheduled",
      tracking-number: "TRK001",
      cost: cost
    })
    
    (ok transport-id)
  )
)

;; Process customs clearance
(define-public (process-customs-clearance
  (cargo-id uint)
  (customs-reference (string-ascii 32))
  (declaration-type (string-ascii 16))
  (submitted-documents (list 10 (string-ascii 32)))
  (duties-paid bool)
)
  (begin
    (asserts! (default-to false (map-get? customs-officers tx-sender)) ERR-UNAUTHORIZED)
    (asserts! (is-some (map-get? cargo-manifests cargo-id)) ERR-NOT-FOUND)
    
    (map-set customs-clearance cargo-id {
      customs-reference: customs-reference,
      declaration-type: declaration-type,
      submitted-documents: submitted-documents,
      clearance-status: (if duties-paid "cleared" "pending"),
      duties-paid: duties-paid,
      inspection-required: (not duties-paid),
      clearance-date: burn-block-height,
      customs-officer: "OFFICER001"
    })
    
    (ok true)
  )
)

;; Complete cargo operation
(define-public (complete-cargo-operation
  (operation-id uint)
  (containers-processed uint)
  (efficiency-score uint)
)
  (let ((operation (map-get? cargo-operations operation-id)))
    (asserts! (is-some operation) ERR-NOT-FOUND)
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    
    (let ((operation-data (unwrap-panic operation)))
      (asserts! (is-eq (get operation-status operation-data) "scheduled") ERR-INVALID-PARAMS)
      
      (map-set cargo-operations operation-id
        (merge operation-data {
          actual-completion: burn-block-height,
          containers-processed: containers-processed,
          operation-status: "completed",
          efficiency-score: efficiency-score
        })
      )
      
      (ok true)
    )
  )
)

;; Update equipment performance
(define-public (update-equipment-performance
  (equipment-id (string-ascii 16))
  (equipment-type (string-ascii 32))
  (utilization-rate uint)
  (efficiency-rating uint)
  (maintenance-status (string-ascii 16))
)
  (begin
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    (asserts! (<= utilization-rate u100) ERR-INVALID-PARAMS)
    
    (map-set equipment-performance equipment-id {
      equipment-type: equipment-type,
      daily-utilization: utilization-rate,
      maintenance-status: maintenance-status,
      breakdown-incidents: u0,
      efficiency-rating: efficiency-rating,
      last-maintenance: burn-block-height,
      next-maintenance: (+ burn-block-height u1440)  ;; Next maintenance in 10 days
    })
    
    (ok true)
  )
)

;; Optimize cargo flow
(define-public (optimize-cargo-flow
  (entry-gate (string-ascii 16))
  (exit-gate (string-ascii 16))
  (processing-time uint)
  (queue-length uint)
  (bottleneck-points (list 5 (string-ascii 32)))
)
  (let ((flow-id burn-block-height))
    (asserts! (is-authorized tx-sender) ERR-UNAUTHORIZED)
    
    (let 
      (
        (throughput-rate (if (> processing-time u0) (/ u3600 processing-time) u0))
        (optimization-score (- u100 (/ (* queue-length u10) u1)))
      )
      (map-set cargo-flow flow-id {
        entry-gate: entry-gate,
        exit-gate: exit-gate,
        processing-time: processing-time,
        queue-length: queue-length,
        bottleneck-points: bottleneck-points,
        optimization-score: optimization-score,
        throughput-rate: throughput-rate
      })
      
      (ok flow-id)
    )
  )
)

;; Authorize cargo handler
(define-public (authorize-handler (handler principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set authorized-handlers handler true)
    (ok true)
  )
)

;; Authorize customs officer
(define-public (authorize-customs-officer (officer principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-OWNER-ONLY)
    (map-set customs-officers officer true)
    (ok true)
  )
)

;; Read-only Functions
(define-read-only (get-cargo-manifest (cargo-id uint))
  (map-get? cargo-manifests cargo-id)
)

(define-read-only (get-container (container-number (string-ascii 16)))
  (map-get? containers container-number)
)

(define-read-only (get-cargo-operation (operation-id uint))
  (map-get? cargo-operations operation-id)
)

(define-read-only (get-transport-coordination (transport-id uint))
  (map-get? transport-coordination transport-id)
)

(define-read-only (get-customs-clearance (cargo-id uint))
  (map-get? customs-clearance cargo-id)
)

(define-read-only (get-equipment-performance (equipment-id (string-ascii 16)))
  (map-get? equipment-performance equipment-id)
)

(define-read-only (calculate-cargo-processing-time (cargo-id uint))
  (match (map-get? cargo-manifests cargo-id)
    cargo-data
    (ok (calculate-processing-time 
      (get weight cargo-data) 
      (get cargo-type cargo-data)
    ))
    ERR-NOT-FOUND
  )
)


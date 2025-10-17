;; Cargo Handling Coordinator
;; Smart contract for coordinating cargo handling and logistics operations
;; Manages container movements, loading/unloading, and transport integration

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u200))
(define-constant err-not-found (err u201))
(define-constant err-already-exists (err u202))
(define-constant err-invalid-container (err u203))
(define-constant err-container-in-use (err u204))
(define-constant err-invalid-operation (err u205))
(define-constant err-unauthorized-access (err u206))
(define-constant err-capacity-exceeded (err u207))
(define-constant err-invalid-status (err u208))
(define-constant err-transport-unavailable (err u209))

;; Data Variables
(define-data-var next-container-id uint u1)
(define-data-var next-operation-id uint u1)
(define-data-var next-transport-id uint u1)
(define-data-var port-coordinator principal tx-sender)
(define-data-var total-handling-capacity uint u1000)
(define-data-var current-load uint u0)

;; Data Maps
(define-map containers
    { container-id: uint }
    {
        container-number: (string-ascii 20),
        container-type: (string-ascii 15),
        size: (string-ascii 10),
        weight: uint,
        cargo-type: (string-ascii 30),
        owner: principal,
        vessel-id: (optional uint),
        current-location: (string-ascii 50),
        destination: (string-ascii 50),
        status: (string-ascii 20),
        handling-priority: uint,
        created-at: uint,
        last-updated: uint
    }
)

(define-map handling-operations
    { operation-id: uint }
    {
        operation-type: (string-ascii 20),
        container-id: uint,
        vessel-id: (optional uint),
        operator: principal,
        start-time: uint,
        end-time: (optional uint),
        status: (string-ascii 15),
        equipment-used: (string-ascii 30),
        efficiency-score: uint,
        created-at: uint
    }
)

(define-map transport-coordination
    { transport-id: uint }
    {
        transport-type: (string-ascii 15),
        container-ids: (list 10 uint),
        origin: (string-ascii 50),
        destination: (string-ascii 50),
        transport-operator: principal,
        scheduled-pickup: uint,
        actual-pickup: (optional uint),
        delivery-time: (optional uint),
        status: (string-ascii 20),
        capacity-utilized: uint,
        created-at: uint
    }
)

(define-map cargo-tracking
    { container-id: uint, checkpoint: (string-ascii 30) }
    {
        timestamp: uint,
        location: (string-ascii 50),
        handler: principal,
        status: (string-ascii 20),
        notes: (string-ascii 100)
    }
)

(define-map equipment-utilization
    { equipment-type: (string-ascii 30) }
    {
        total-units: uint,
        available-units: uint,
        utilization-rate: uint,
        maintenance-count: uint,
        last-updated: uint
    }
)

(define-map performance-metrics
    { metric-type: (string-ascii 30), time-period: uint }
    {
        value: uint,
        target: uint,
        efficiency: uint,
        recorded-by: principal,
        timestamp: uint
    }
)

;; Private Functions
(define-private (is-contract-owner)
    (is-eq tx-sender contract-owner)
)

(define-private (is-port-coordinator)
    (is-eq tx-sender (var-get port-coordinator))
)

(define-private (calculate-handling-priority (cargo-type (string-ascii 30)) (container-type (string-ascii 15)))
    (if (is-eq cargo-type "hazardous")
        u10
        (if (is-eq cargo-type "perishable")
            u9
            (if (is-eq container-type "reefer")
                u8
                (if (is-eq cargo-type "fragile")
                    u7
                    u5
                )
            )
        )
    )
)

(define-private (update-equipment-availability (equipment-type (string-ascii 30)) (change int))
    (match (map-get? equipment-utilization { equipment-type: equipment-type })
        equipment-data
        (let ((current-available (get available-units equipment-data))
              (new-available (if (> change 0) 
                              (+ current-available (to-uint change))
                              (- current-available (to-uint (* change -1))))))
            (map-set equipment-utilization
                { equipment-type: equipment-type }
                (merge equipment-data {
                    available-units: new-available,
                    utilization-rate: (/ (* (- (get total-units equipment-data) new-available) u100)
                                       (get total-units equipment-data)),
                    last-updated: burn-block-height
                })
            )
            true
        )
        false
    )
)

(define-private (calculate-operation-efficiency (operation-type (string-ascii 20)) (duration uint))
    (let ((standard-times (if (is-eq operation-type "loading")
                           u120
                           (if (is-eq operation-type "unloading")
                               u100
                               u80))))
        (if (<= duration standard-times)
            (/ (* u100 standard-times) duration)
            (/ (* u100 duration) standard-times)
        )
    )
)

(define-private (validate-container-status (status (string-ascii 20)))
    (or (is-eq status "arriving")
        (is-eq status "unloading")
        (is-eq status "in-yard")
        (is-eq status "loading")
        (is-eq status "departed")
        (is-eq status "in-transit")
    )
)

;; Public Functions
(define-public (register-container
    (container-number (string-ascii 20))
    (container-type (string-ascii 15))
    (size (string-ascii 10))
    (weight uint)
    (cargo-type (string-ascii 30))
    (vessel-id (optional uint))
    (current-location (string-ascii 50))
    (destination (string-ascii 50))
    )
    (let ((container-id (var-get next-container-id))
          (priority (calculate-handling-priority cargo-type container-type)))
        (asserts! (> weight u0) err-invalid-container)
        (asserts! (< weight u50000) err-capacity-exceeded)
        
        (map-set containers
            { container-id: container-id }
            {
                container-number: container-number,
                container-type: container-type,
                size: size,
                weight: weight,
                cargo-type: cargo-type,
                owner: tx-sender,
                vessel-id: vessel-id,
                current-location: current-location,
                destination: destination,
                status: "arriving",
                handling-priority: priority,
                created-at: burn-block-height,
                last-updated: burn-block-height
            }
        )
        
        (var-set next-container-id (+ container-id u1))
        (ok container-id)
    )
)

(define-public (create-handling-operation
    (operation-type (string-ascii 20))
    (container-id uint)
    (vessel-id (optional uint))
    (equipment-used (string-ascii 30))
    )
    (let ((operation-id (var-get next-operation-id))
          (container-data (unwrap! (map-get? containers { container-id: container-id }) err-not-found)))
        (asserts! (or (is-port-coordinator) (is-eq tx-sender (get owner container-data))) err-unauthorized-access)
        (asserts! (or (is-eq operation-type "loading") 
                      (is-eq operation-type "unloading") 
                      (is-eq operation-type "transfer")) err-invalid-operation)
        
        ;; Update equipment availability
        (update-equipment-availability equipment-used -1)
        
        (map-set handling-operations
            { operation-id: operation-id }
            {
                operation-type: operation-type,
                container-id: container-id,
                vessel-id: vessel-id,
                operator: tx-sender,
                start-time: burn-block-height,
                end-time: none,
                status: "in-progress",
                equipment-used: equipment-used,
                efficiency-score: u0,
                created-at: burn-block-height
            }
        )
        
        ;; Update container status
        (map-set containers
            { container-id: container-id }
            (merge container-data {
                status: operation-type,
                last-updated: burn-block-height
            })
        )
        
        (var-set next-operation-id (+ operation-id u1))
        (var-set current-load (+ (var-get current-load) u1))
        (ok operation-id)
    )
)

(define-public (complete-handling-operation (operation-id uint))
    (let ((operation-data (unwrap! (map-get? handling-operations { operation-id: operation-id }) err-not-found))
          (duration (- burn-block-height (get start-time operation-data)))
          (efficiency (calculate-operation-efficiency (get operation-type operation-data) duration)))
        (asserts! (is-eq tx-sender (get operator operation-data)) err-unauthorized-access)
        (asserts! (is-eq (get status operation-data) "in-progress") err-invalid-status)
        
        ;; Update operation
        (map-set handling-operations
            { operation-id: operation-id }
            (merge operation-data {
                end-time: (some burn-block-height),
                status: "completed",
                efficiency-score: efficiency
            })
        )
        
        ;; Release equipment
        (update-equipment-availability (get equipment-used operation-data) 1)
        
        ;; Update container status based on operation type
        (let ((container-data (unwrap! (map-get? containers { container-id: (get container-id operation-data) }) err-not-found))
              (new-status (if (is-eq (get operation-type operation-data) "unloading")
                            "in-yard"
                            (if (is-eq (get operation-type operation-data) "loading")
                                "loaded"
                                "transferred"
                            ))))
            (map-set containers
                { container-id: (get container-id operation-data) }
                (merge container-data {
                    status: new-status,
                    last-updated: burn-block-height
                })
            )
        )
        
        (var-set current-load (- (var-get current-load) u1))
        (ok true)
    )
)

(define-public (coordinate-transport
    (transport-type (string-ascii 15))
    (container-ids (list 10 uint))
    (origin (string-ascii 50))
    (destination (string-ascii 50))
    (scheduled-pickup uint)
    )
    (let ((transport-id (var-get next-transport-id)))
        (asserts! (is-port-coordinator) err-unauthorized-access)
        (asserts! (> scheduled-pickup burn-block-height) err-invalid-operation)
        (asserts! (> (len container-ids) u0) err-invalid-container)
        
        (map-set transport-coordination
            { transport-id: transport-id }
            {
                transport-type: transport-type,
                container-ids: container-ids,
                origin: origin,
                destination: destination,
                transport-operator: tx-sender,
                scheduled-pickup: scheduled-pickup,
                actual-pickup: none,
                delivery-time: none,
                status: "scheduled",
                capacity-utilized: (len container-ids),
                created-at: burn-block-height
            }
        )
        
        (var-set next-transport-id (+ transport-id u1))
        (ok transport-id)
    )
)

(define-public (track-cargo-movement
    (container-id uint)
    (checkpoint (string-ascii 30))
    (location (string-ascii 50))
    (status (string-ascii 20))
    (notes (string-ascii 100))
    )
    (let ((container-data (unwrap! (map-get? containers { container-id: container-id }) err-not-found)))
        (asserts! (or (is-port-coordinator) (is-eq tx-sender (get owner container-data))) err-unauthorized-access)
        (asserts! (validate-container-status status) err-invalid-status)
        
        (map-set cargo-tracking
            { container-id: container-id, checkpoint: checkpoint }
            {
                timestamp: burn-block-height,
                location: location,
                handler: tx-sender,
                status: status,
                notes: notes
            }
        )
        
        ;; Update container location and status
        (map-set containers
            { container-id: container-id }
            (merge container-data {
                current-location: location,
                status: status,
                last-updated: burn-block-height
            })
        )
        
        (ok true)
    )
)

(define-public (update-equipment-inventory
    (equipment-type (string-ascii 30))
    (total-units uint)
    (available-units uint)
    (maintenance-count uint)
    )
    (begin
        (asserts! (is-port-coordinator) err-unauthorized-access)
        (asserts! (<= available-units total-units) err-invalid-operation)
        
        (map-set equipment-utilization
            { equipment-type: equipment-type }
            {
                total-units: total-units,
                available-units: available-units,
                utilization-rate: (/ (* (- total-units available-units) u100) total-units),
                maintenance-count: maintenance-count,
                last-updated: burn-block-height
            }
        )
        
        (ok true)
    )
)

(define-public (record-performance-metric
    (metric-type (string-ascii 30))
    (time-period uint)
    (value uint)
    (target uint)
    )
    (begin
        (asserts! (is-port-coordinator) err-unauthorized-access)
        
        (map-set performance-metrics
            { metric-type: metric-type, time-period: time-period }
            {
                value: value,
                target: target,
                efficiency: (if (> target u0) (/ (* value u100) target) u0),
                recorded-by: tx-sender,
                timestamp: burn-block-height
            }
        )
        
        (ok true)
    )
)

;; Read-only Functions
(define-read-only (get-container (container-id uint))
    (map-get? containers { container-id: container-id })
)

(define-read-only (get-handling-operation (operation-id uint))
    (map-get? handling-operations { operation-id: operation-id })
)

(define-read-only (get-transport-info (transport-id uint))
    (map-get? transport-coordination { transport-id: transport-id })
)

(define-read-only (get-cargo-tracking (container-id uint) (checkpoint (string-ascii 30)))
    (map-get? cargo-tracking { container-id: container-id, checkpoint: checkpoint })
)

(define-read-only (get-equipment-status (equipment-type (string-ascii 30)))
    (map-get? equipment-utilization { equipment-type: equipment-type })
)

(define-read-only (get-performance-data (metric-type (string-ascii 30)) (time-period uint))
    (map-get? performance-metrics { metric-type: metric-type, time-period: time-period })
)

(define-read-only (get-operational-capacity)
    {
        total-capacity: (var-get total-handling-capacity),
        current-load: (var-get current-load),
        available-capacity: (- (var-get total-handling-capacity) (var-get current-load)),
        utilization-rate: (/ (* (var-get current-load) u100) (var-get total-handling-capacity))
    }
)

(define-read-only (get-contract-info)
    {
        owner: contract-owner,
        coordinator: (var-get port-coordinator),
        next-container-id: (var-get next-container-id),
        next-operation-id: (var-get next-operation-id),
        next-transport-id: (var-get next-transport-id)
    }
)


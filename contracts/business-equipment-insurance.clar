
;; title: business-equipment-insurance
;; version: 1.0
;; summary: Equipment Insurance for Small Business - Commercial property coverage system with asset valuation, claim processing, and replacement coordination
;; description: A comprehensive smart contract for managing equipment insurance policies, claims processing, and asset valuation for small businesses

;; constants
(define-constant CONTRACT-OWNER tx-sender)
(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-POLICY-NOT-FOUND (err u101))
(define-constant ERR-EQUIPMENT-NOT-FOUND (err u102))
(define-constant ERR-CLAIM-NOT-FOUND (err u103))
(define-constant ERR-INSUFFICIENT-PAYMENT (err u104))
(define-constant ERR-CLAIM-ALREADY-PROCESSED (err u105))
(define-constant ERR-POLICY-EXPIRED (err u106))
(define-constant ERR-INVALID-VALUE (err u107))

;; data vars
(define-data-var policy-counter uint u0)
(define-data-var equipment-counter uint u0)
(define-data-var claim-counter uint u0)

;; data maps
(define-map policies uint {
    business-owner: principal,
    premium-amount: uint,
    coverage-limit: uint,
    deductible: uint,
    policy-start: uint,
    policy-end: uint,
    is-active: bool
})

(define-map equipment uint {
    policy-id: uint,
    name: (string-ascii 50),
    category: (string-ascii 30),
    purchase-date: uint,
    purchase-value: uint,
    current-value: uint,
    serial-number: (string-ascii 50),
    is-covered: bool
})

(define-map claims uint {
    policy-id: uint,
    equipment-id: uint,
    claim-amount: uint,
    claim-date: uint,
    damage-description: (string-ascii 200),
    status: (string-ascii 20),
    approved-amount: uint,
    processed-date: (optional uint)
})

(define-map policy-equipment uint (list 50 uint))

;; public functions

;; Create a new insurance policy
(define-public (create-policy (premium uint) (coverage-limit uint) (deductible uint) (duration uint))
    (let ((policy-id (+ (var-get policy-counter) u1)))
        (asserts! (> premium u0) ERR-INVALID-VALUE)
        (asserts! (> coverage-limit u0) ERR-INVALID-VALUE)
        (asserts! (> duration u0) ERR-INVALID-VALUE)
        
        (map-set policies policy-id {
            business-owner: tx-sender,
            premium-amount: premium,
            coverage-limit: coverage-limit,
            deductible: deductible,
            policy-start: stacks-block-height,
            policy-end: (+ stacks-block-height duration),
            is-active: true
        })
        
        (map-set policy-equipment policy-id (list))
        (var-set policy-counter policy-id)
        (ok policy-id)
    )
)

;; Add equipment to a policy
(define-public (add-equipment (policy-id uint) (name (string-ascii 50)) (category (string-ascii 30)) 
                             (purchase-value uint) (serial-number (string-ascii 50)))
    (let ((equipment-id (+ (var-get equipment-counter) u1))
          (policy (unwrap! (map-get? policies policy-id) ERR-POLICY-NOT-FOUND))
          (current-equipment (default-to (list) (map-get? policy-equipment policy-id))))
        
        (asserts! (is-eq (get business-owner policy) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active policy) ERR-POLICY-EXPIRED)
        (asserts! (> purchase-value u0) ERR-INVALID-VALUE)
        
        (map-set equipment equipment-id {
            policy-id: policy-id,
            name: name,
            category: category,
            purchase-date: stacks-block-height,
            purchase-value: purchase-value,
            current-value: purchase-value,
            serial-number: serial-number,
            is-covered: true
        })
        
        (map-set policy-equipment policy-id (unwrap! (as-max-len? (append current-equipment equipment-id) u50) ERR-INVALID-VALUE))
        (var-set equipment-counter equipment-id)
        (ok equipment-id)
    )
)

;; Update equipment current value for depreciation
(define-public (update-equipment-value (equipment-id uint) (new-value uint))
    (let ((equipment-data (unwrap! (map-get? equipment equipment-id) ERR-EQUIPMENT-NOT-FOUND))
          (policy (unwrap! (map-get? policies (get policy-id equipment-data)) ERR-POLICY-NOT-FOUND)))
        
        (asserts! (is-eq (get business-owner policy) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (> new-value u0) ERR-INVALID-VALUE)
        
        (map-set equipment equipment-id (merge equipment-data { current-value: new-value }))
        (ok true)
    )
)

;; File a claim
(define-public (file-claim (policy-id uint) (equipment-id uint) (claim-amount uint) (damage-description (string-ascii 200)))
    (let ((claim-id (+ (var-get claim-counter) u1))
          (policy (unwrap! (map-get? policies policy-id) ERR-POLICY-NOT-FOUND))
          (equipment-data (unwrap! (map-get? equipment equipment-id) ERR-EQUIPMENT-NOT-FOUND)))
        
        (asserts! (is-eq (get business-owner policy) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (get is-active policy) ERR-POLICY-EXPIRED)
        (asserts! (< stacks-block-height (get policy-end policy)) ERR-POLICY-EXPIRED)
        (asserts! (is-eq (get policy-id equipment-data) policy-id) ERR-EQUIPMENT-NOT-FOUND)
        (asserts! (get is-covered equipment-data) ERR-EQUIPMENT-NOT-FOUND)
        (asserts! (> claim-amount u0) ERR-INVALID-VALUE)
        
        (map-set claims claim-id {
            policy-id: policy-id,
            equipment-id: equipment-id,
            claim-amount: claim-amount,
            claim-date: stacks-block-height,
            damage-description: damage-description,
            status: "pending",
            approved-amount: u0,
            processed-date: none
        })
        
        (var-set claim-counter claim-id)
        (ok claim-id)
    )
)

;; Process claim (admin function)
(define-public (process-claim (claim-id uint) (approved-amount uint) (approve bool))
    (let ((claim-data (unwrap! (map-get? claims claim-id) ERR-CLAIM-NOT-FOUND))
          (policy (unwrap! (map-get? policies (get policy-id claim-data)) ERR-POLICY-NOT-FOUND)))
        
        (asserts! (is-eq tx-sender CONTRACT-OWNER) ERR-NOT-AUTHORIZED)
        (asserts! (is-eq (get status claim-data) "pending") ERR-CLAIM-ALREADY-PROCESSED)
        (asserts! (<= approved-amount (get coverage-limit policy)) ERR-INVALID-VALUE)
        
        (map-set claims claim-id (merge claim-data {
            status: (if approve "approved" "denied"),
            approved-amount: (if approve approved-amount u0),
            processed-date: (some stacks-block-height)
        }))
        
        (ok true)
    )
)

;; Pay premium to renew or maintain policy
(define-public (pay-premium (policy-id uint) (payment uint))
    (let ((policy (unwrap! (map-get? policies policy-id) ERR-POLICY-NOT-FOUND)))
        (asserts! (is-eq (get business-owner policy) tx-sender) ERR-NOT-AUTHORIZED)
        (asserts! (>= payment (get premium-amount policy)) ERR-INSUFFICIENT-PAYMENT)
        
        ;; In a real implementation, this would handle STX transfer
        ;; For this demo, we just mark the policy as active
        (map-set policies policy-id (merge policy { is-active: true }))
        (ok true)
    )
)

;; read only functions

;; Get policy details
(define-read-only (get-policy (policy-id uint))
    (map-get? policies policy-id)
)

;; Get equipment details
(define-read-only (get-equipment (equipment-id uint))
    (map-get? equipment equipment-id)
)

;; Get claim details
(define-read-only (get-claim (claim-id uint))
    (map-get? claims claim-id)
)

;; Get all equipment for a policy
(define-read-only (get-policy-equipment (policy-id uint))
    (map-get? policy-equipment policy-id)
)

;; Calculate depreciated value
(define-read-only (calculate-depreciated-value (equipment-id uint) (depreciation-rate uint))
    (match (map-get? equipment equipment-id)
        equipment-data
        (let ((age-in-blocks (- stacks-block-height (get purchase-date equipment-data)))
              (original-value (get purchase-value equipment-data))
              (depreciated-amount (* original-value depreciation-rate (/ age-in-blocks u52560)))
              (depreciated-value (if (> depreciated-amount original-value) u1 (- original-value depreciated-amount))))
            (ok (if (< depreciated-value u1) u1 depreciated-value)))
        ERR-EQUIPMENT-NOT-FOUND
    )
)

;; Get policy status
(define-read-only (is-policy-active (policy-id uint))
    (match (map-get? policies policy-id)
        policy-data
        (and (get is-active policy-data) (< stacks-block-height (get policy-end policy-data)))
        false
    )
)

;; Get total coverage for a policy
(define-read-only (get-total-coverage-used (policy-id uint))
    (match (map-get? policy-equipment policy-id)
        equipment-list
        (fold calculate-equipment-value equipment-list u0)
        u0
    )
)

;; private functions

;; Helper function for calculating total equipment value
(define-private (calculate-equipment-value (equipment-id uint) (current-total uint))
    (match (map-get? equipment equipment-id)
        equipment-data
        (if (get is-covered equipment-data)
            (+ current-total (get current-value equipment-data))
            current-total)
        current-total
    )
)


;; SummitForge - Decentralized Creator Economy Platform
;; A platform for dynamic royalty streams and fractional IP ownership

;; Constants
(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-found (err u101))
(define-constant err-unauthorized (err u102))
(define-constant err-already-exists (err u103))
(define-constant err-invalid-amount (err u104))
(define-constant err-invalid-percentage (err u105))
(define-constant err-license-expired (err u106))
(define-constant err-insufficient-balance (err u107))

;; Data Variables
(define-data-var platform-fee-percentage uint u250) ;; 2.5% (basis points)
(define-data-var next-ip-id uint u1)
(define-data-var next-license-id uint u1)

;; Data Maps

;; IP Asset Registry with creation proof
(define-map ip-assets
    uint
    {
        creator: principal,
        title: (string-ascii 256),
        content-hash: (buff 32),
        creation-timestamp: uint,
        total-supply: uint,
        royalty-percentage: uint,
        is-active: bool
    }
)

;; Fractional IP Ownership
(define-map ip-ownership
    {ip-id: uint, owner: principal}
    {shares: uint}
)

;; Licensing Terms
(define-map licenses
    uint
    {
        ip-id: uint,
        licensee: principal,
        license-type: (string-ascii 50),
        territory: (string-ascii 100),
        start-block: uint,
        end-block: uint,
        price: uint,
        is-active: bool
    }
)

;; Royalty Distribution Records
(define-map royalty-distributions
    {ip-id: uint, distribution-id: uint}
    {
        amount: uint,
        timestamp: uint,
        source: (string-ascii 100)
    }
)

;; Creator Reputation System
(define-map creator-reputation
    principal
    {
        total-ips: uint,
        total-licenses: uint,
        total-earnings: uint,
        compliance-score: uint,
        last-updated: uint
    }
)

;; Royalty Beneficiaries (for multi-tiered distributions)
(define-map royalty-beneficiaries
    {ip-id: uint, beneficiary: principal}
    {percentage: uint}
)

;; Platform Revenue Tracking
(define-map platform-revenue
    uint
    {total-collected: uint}
)

;; Read-only functions

(define-read-only (get-ip-asset (ip-id uint))
    (map-get? ip-assets ip-id)
)

(define-read-only (get-ip-ownership (ip-id uint) (owner principal))
    (map-get? ip-ownership {ip-id: ip-id, owner: owner})
)

(define-read-only (get-license (license-id uint))
    (map-get? licenses license-id)
)

(define-read-only (get-creator-reputation (creator principal))
    (default-to 
        {total-ips: u0, total-licenses: u0, total-earnings: u0, compliance-score: u100, last-updated: u0}
        (map-get? creator-reputation creator)
    )
)

(define-read-only (get-royalty-beneficiary (ip-id uint) (beneficiary principal))
    (map-get? royalty-beneficiaries {ip-id: ip-id, beneficiary: beneficiary})
)

(define-read-only (get-platform-fee)
    (var-get platform-fee-percentage)
)

(define-read-only (calculate-platform-fee (amount uint))
    (/ (* amount (var-get platform-fee-percentage)) u10000)
)

(define-read-only (is-license-valid (license-id uint))
    (match (map-get? licenses license-id)
        license-data 
            (and 
                (get is-active license-data)
                (>= block-height (get start-block license-data))
                (<= block-height (get end-block license-data))
            )
        false
    )
)

;; Public functions

;; Mint IP Asset with Proof-of-Creation
(define-public (mint-ip-asset 
    (title (string-ascii 256))
    (content-hash (buff 32))
    (total-supply uint)
    (royalty-percentage uint)
)
    (let
        (
            (ip-id (var-get next-ip-id))
            (creator tx-sender)
        )
        (asserts! (<= royalty-percentage u10000) err-invalid-percentage)
        (asserts! (> total-supply u0) err-invalid-amount)
        
        ;; Create IP asset
        (map-set ip-assets ip-id {
            creator: creator,
            title: title,
            content-hash: content-hash,
            creation-timestamp: block-height,
            total-supply: total-supply,
            royalty-percentage: royalty-percentage,
            is-active: true
        })
        
        ;; Assign full ownership to creator
        (map-set ip-ownership 
            {ip-id: ip-id, owner: creator}
            {shares: total-supply}
        )
        
        ;; Update creator reputation
        (update-creator-reputation creator u1 u0 u0)
        
        ;; Increment IP ID
        (var-set next-ip-id (+ ip-id u1))
        
        (ok ip-id)
    )
)

;; Transfer IP Ownership Shares
(define-public (transfer-ip-shares 
    (ip-id uint)
    (recipient principal)
    (shares uint)
)
    (let
        (
            (sender tx-sender)
            (sender-balance (default-to {shares: u0} 
                (map-get? ip-ownership {ip-id: ip-id, owner: sender})))
            (recipient-balance (default-to {shares: u0} 
                (map-get? ip-ownership {ip-id: ip-id, owner: recipient})))
        )
        (asserts! (>= (get shares sender-balance) shares) err-insufficient-balance)
        (asserts! (> shares u0) err-invalid-amount)
        
        ;; Update sender balance
        (map-set ip-ownership 
            {ip-id: ip-id, owner: sender}
            {shares: (- (get shares sender-balance) shares)}
        )
        
        ;; Update recipient balance
        (map-set ip-ownership 
            {ip-id: ip-id, owner: recipient}
            {shares: (+ (get shares recipient-balance) shares)}
        )
        
        (ok true)
    )
)

;; Create License
(define-public (create-license
    (ip-id uint)
    (license-type (string-ascii 50))
    (territory (string-ascii 100))
    (duration uint)
    (price uint)
)
    (let
        (
            (license-id (var-get next-license-id))
            (ip-asset (unwrap! (map-get? ip-assets ip-id) err-not-found))
            (licensee tx-sender)
        )
        (asserts! (get is-active ip-asset) err-not-found)
        (asserts! (> price u0) err-invalid-amount)
        (asserts! (> duration u0) err-invalid-amount)
        
        ;; Create license
        (map-set licenses license-id {
            ip-id: ip-id,
            licensee: licensee,
            license-type: license-type,
            territory: territory,
            start-block: block-height,
            end-block: (+ block-height duration),
            price: price,
            is-active: true
        })
        
        ;; Distribute payment
        (try! (distribute-license-payment ip-id price (get creator ip-asset)))
        
        ;; Update creator reputation
        (update-creator-reputation (get creator ip-asset) u0 u1 price)
        
        ;; Increment license ID
        (var-set next-license-id (+ license-id u1))
        
        (ok license-id)
    )
)

;; Distribute License Payment with Royalty Splits
(define-private (distribute-license-payment 
    (ip-id uint)
    (amount uint)
    (primary-creator principal)
)
    (let
        (
            (platform-fee (calculate-platform-fee amount))
            (creator-amount (- amount platform-fee))
        )
        ;; Transfer platform fee
        (try! (stx-transfer? platform-fee tx-sender contract-owner))
        
        ;; Transfer creator amount
        (try! (stx-transfer? creator-amount tx-sender primary-creator))
        
        (ok true)
    )
)

;; Add Royalty Beneficiary for Multi-tiered Distribution
(define-public (add-royalty-beneficiary
    (ip-id uint)
    (beneficiary principal)
    (percentage uint)
)
    (let
        (
            (ip-asset (unwrap! (map-get? ip-assets ip-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get creator ip-asset)) err-unauthorized)
        (asserts! (<= percentage u10000) err-invalid-percentage)
        
        (map-set royalty-beneficiaries 
            {ip-id: ip-id, beneficiary: beneficiary}
            {percentage: percentage}
        )
        
        (ok true)
    )
)

;; Distribute Royalties to Multiple Beneficiaries
(define-public (distribute-royalties
    (ip-id uint)
    (amount uint)
)
    (let
        (
            (ip-asset (unwrap! (map-get? ip-assets ip-id) err-not-found))
            (platform-fee (calculate-platform-fee amount))
            (distributable-amount (- amount platform-fee))
        )
        (asserts! (is-eq tx-sender (get creator ip-asset)) err-unauthorized)
        (asserts! (> amount u0) err-invalid-amount)
        
        ;; Transfer platform fee
        (try! (stx-transfer? platform-fee tx-sender contract-owner))
        
        ;; Note: In production, you would iterate through beneficiaries
        ;; For simplicity, sending to creator
        (try! (stx-transfer? distributable-amount tx-sender (get creator ip-asset)))
        
        (ok true)
    )
)

;; Revoke License
(define-public (revoke-license (license-id uint))
    (let
        (
            (license-data (unwrap! (map-get? licenses license-id) err-not-found))
            (ip-asset (unwrap! (map-get? ip-assets (get ip-id license-data)) err-not-found))
        )
        (asserts! (is-eq tx-sender (get creator ip-asset)) err-unauthorized)
        
        (map-set licenses license-id 
            (merge license-data {is-active: false})
        )
        
        (ok true)
    )
)

;; Update Creator Reputation (internal helper)
(define-private (update-creator-reputation
    (creator principal)
    (ips-delta uint)
    (licenses-delta uint)
    (earnings-delta uint)
)
    (let
        (
            (current-rep (get-creator-reputation creator))
        )
        (map-set creator-reputation creator {
            total-ips: (+ (get total-ips current-rep) ips-delta),
            total-licenses: (+ (get total-licenses current-rep) licenses-delta),
            total-earnings: (+ (get total-earnings current-rep) earnings-delta),
            compliance-score: (get compliance-score current-rep),
            last-updated: block-height
        })
        true
    )
)

;; Update Compliance Score (only contract owner)
(define-public (update-compliance-score
    (creator principal)
    (new-score uint)
)
    (let
        (
            (current-rep (get-creator-reputation creator))
        )
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-score u100) err-invalid-percentage)
        
        (map-set creator-reputation creator 
            (merge current-rep {
                compliance-score: new-score,
                last-updated: block-height
            })
        )
        
        (ok true)
    )
)

;; Set Platform Fee (only contract owner)
(define-public (set-platform-fee (new-fee uint))
    (begin
        (asserts! (is-eq tx-sender contract-owner) err-owner-only)
        (asserts! (<= new-fee u1000) err-invalid-percentage) ;; Max 10%
        (var-set platform-fee-percentage new-fee)
        (ok true)
    )
)

;; Deactivate IP Asset (only creator)
(define-public (deactivate-ip-asset (ip-id uint))
    (let
        (
            (ip-asset (unwrap! (map-get? ip-assets ip-id) err-not-found))
        )
        (asserts! (is-eq tx-sender (get creator ip-asset)) err-unauthorized)
        
        (map-set ip-assets ip-id 
            (merge ip-asset {is-active: false})
        )
        
        (ok true)
    )
)
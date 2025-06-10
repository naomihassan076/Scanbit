;; title: Scanbit
;; version: 1.0.0
;; summary: Smart QR Rewards - Physical scans unlock token incentives
;; description: A decentralized QR code scanning system that rewards users with tokens for scanning physical QR codes

;; traits

;; token definitions
(define-fungible-token scanbit-token)

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_QR_NOT_FOUND (err u101))
(define-constant ERR_QR_ALREADY_SCANNED (err u102))
(define-constant ERR_QR_EXPIRED (err u103))
(define-constant ERR_INSUFFICIENT_BALANCE (err u104))
(define-constant ERR_INVALID_REWARD (err u105))
(define-constant ERR_QR_INACTIVE (err u106))
(define-constant ERR_DAILY_LIMIT_EXCEEDED (err u107))
(define-constant ERR_COOLDOWN_ACTIVE (err u108))

(define-constant INITIAL_TOKEN_SUPPLY u1000000000000)
(define-constant MAX_DAILY_SCANS u10)
(define-constant SCAN_COOLDOWN u144)
(define-constant BLOCKS_PER_DAY u144)

;; data vars
(define-data-var next-qr-id uint u1)
(define-data-var total-scans uint u0)
(define-data-var contract-balance uint u0)

;; data maps
(define-map qr-codes
  { qr-id: uint }
  {
    creator: principal,
    reward-amount: uint,
    max-scans: uint,
    current-scans: uint,
    expiry-block: uint,
    active: bool,
    location: (string-ascii 100),
    category: (string-ascii 50)
  }
)

(define-map user-scans
  { user: principal, qr-id: uint }
  {
    scanned-at: uint,
    reward-earned: uint
  }
)

(define-map daily-scan-count
  { user: principal, day: uint }
  { count: uint }
)

(define-map user-stats
  { user: principal }
  {
    total-scans: uint,
    total-rewards: uint,
    last-scan-block: uint,
    reputation-score: uint
  }
)

(define-map qr-scan-history
  { qr-id: uint, scan-number: uint }
  {
    scanner: principal,
    scanned-at: uint,
    reward-amount: uint
  }
)

;; public functions

(define-public (create-qr-code (reward-amount uint) (max-scans uint) (duration-blocks uint) (location (string-ascii 100)) (category (string-ascii 50)))
  (let
    (
      (qr-id (var-get next-qr-id))
      (expiry-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (> reward-amount u0) ERR_INVALID_REWARD)
    (asserts! (> max-scans u0) ERR_INVALID_REWARD)
    (asserts! (> duration-blocks u0) ERR_INVALID_REWARD)
    
    (map-set qr-codes
      { qr-id: qr-id }
      {
        creator: tx-sender,
        reward-amount: reward-amount,
        max-scans: max-scans,
        current-scans: u0,
        expiry-block: expiry-block,
        active: true,
        location: location,
        category: category
      }
    )
    
    (var-set next-qr-id (+ qr-id u1))
    (ok qr-id)
  )
)

(define-public (scan-qr-code (qr-id uint))
  (let
    (
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
      (current-day (/ stacks-block-height BLOCKS_PER_DAY))
      (daily-scans (default-to { count: u0 } (map-get? daily-scan-count { user: tx-sender, day: current-day })))
      (user-scan-data (map-get? user-scans { user: tx-sender, qr-id: qr-id }))
      (user-data (default-to { total-scans: u0, total-rewards: u0, last-scan-block: u0, reputation-score: u0 } 
                              (map-get? user-stats { user: tx-sender })))
    )
    
    (asserts! (get active qr-data) ERR_QR_INACTIVE)
    (asserts! (< stacks-block-height (get expiry-block qr-data)) ERR_QR_EXPIRED)
    (asserts! (< (get current-scans qr-data) (get max-scans qr-data)) ERR_QR_ALREADY_SCANNED)
    (asserts! (is-none user-scan-data) ERR_QR_ALREADY_SCANNED)
    (asserts! (< (get count daily-scans) MAX_DAILY_SCANS) ERR_DAILY_LIMIT_EXCEEDED)
    (asserts! (>= stacks-block-height (+ (get last-scan-block user-data) SCAN_COOLDOWN)) ERR_COOLDOWN_ACTIVE)
    
    (try! (ft-mint? scanbit-token (get reward-amount qr-data) tx-sender))
    
    (map-set user-scans
      { user: tx-sender, qr-id: qr-id }
      {
        scanned-at: stacks-block-height,
        reward-earned: (get reward-amount qr-data)
      }
    )
    
    (map-set qr-codes
      { qr-id: qr-id }
      (merge qr-data { current-scans: (+ (get current-scans qr-data) u1) })
    )
    
    (map-set daily-scan-count
      { user: tx-sender, day: current-day }
      { count: (+ (get count daily-scans) u1) }
    )
    
    (map-set user-stats
      { user: tx-sender }
      {
        total-scans: (+ (get total-scans user-data) u1),
        total-rewards: (+ (get total-rewards user-data) (get reward-amount qr-data)),
        last-scan-block: stacks-block-height,
        reputation-score: (+ (get reputation-score user-data) u1)
      }
    )
    
    (map-set qr-scan-history
      { qr-id: qr-id, scan-number: (get current-scans qr-data) }
      {
        scanner: tx-sender,
        scanned-at: stacks-block-height,
        reward-amount: (get reward-amount qr-data)
      }
    )
    
    (var-set total-scans (+ (var-get total-scans) u1))
    (ok (get reward-amount qr-data))
  )
)

(define-public (deactivate-qr-code (qr-id uint))
  (let
    (
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator qr-data)) ERR_NOT_AUTHORIZED)
    
    (map-set qr-codes
      { qr-id: qr-id }
      (merge qr-data { active: false })
    )
    (ok true)
  )
)

(define-public (update-qr-reward (qr-id uint) (new-reward uint))
  (let
    (
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator qr-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> new-reward u0) ERR_INVALID_REWARD)
    
    (map-set qr-codes
      { qr-id: qr-id }
      (merge qr-data { reward-amount: new-reward })
    )
    (ok true)
  )
)

(define-public (extend-qr-expiry (qr-id uint) (additional-blocks uint))
  (let
    (
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator qr-data)) ERR_NOT_AUTHORIZED)
    (asserts! (> additional-blocks u0) ERR_INVALID_REWARD)
    
    (map-set qr-codes
      { qr-id: qr-id }
      (merge qr-data { expiry-block: (+ (get expiry-block qr-data) additional-blocks) })
    )
    (ok true)
  )
)

(define-public (transfer-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (> amount u0) ERR_INVALID_REWARD)
    (ft-transfer? scanbit-token amount tx-sender recipient)
  )
)

(define-public (mint-tokens (amount uint) (recipient principal))
  (begin
    (asserts! (is-eq tx-sender CONTRACT_OWNER) ERR_NOT_AUTHORIZED)
    (asserts! (> amount u0) ERR_INVALID_REWARD)
    (ft-mint? scanbit-token amount recipient)
  )
)

;; read only functions

(define-read-only (get-qr-code (qr-id uint))
  (map-get? qr-codes { qr-id: qr-id })
)

(define-read-only (get-user-scan (user principal) (qr-id uint))
  (map-get? user-scans { user: user, qr-id: qr-id })
)

(define-read-only (get-user-stats (user principal))
  (map-get? user-stats { user: user })
)

(define-read-only (get-daily-scan-count (user principal))
  (let
    (
      (current-day (/ stacks-block-height BLOCKS_PER_DAY))
    )
    (default-to { count: u0 } (map-get? daily-scan-count { user: user, day: current-day }))
  )
)

(define-read-only (get-scan-history (qr-id uint) (scan-number uint))
  (map-get? qr-scan-history { qr-id: qr-id, scan-number: scan-number })
)

(define-read-only (get-token-balance (user principal))
  (ft-get-balance scanbit-token user)
)

(define-read-only (get-total-supply)
  (ft-get-supply scanbit-token)
)

(define-read-only (get-contract-stats)
  {
    total-scans: (var-get total-scans),
    next-qr-id: (var-get next-qr-id),
    current-block: stacks-block-height
  }
)

(define-read-only (can-user-scan (user principal) (qr-id uint))
  (let
    (
      (qr-data (map-get? qr-codes { qr-id: qr-id }))
      (current-day (/ stacks-block-height BLOCKS_PER_DAY))
      (daily-scans (default-to { count: u0 } (map-get? daily-scan-count { user: user, day: current-day })))
      (user-scan-data (map-get? user-scans { user: user, qr-id: qr-id }))
      (user-data (default-to { total-scans: u0, total-rewards: u0, last-scan-block: u0, reputation-score: u0 } 
                              (map-get? user-stats { user: user })))
    )
    (match qr-data
      qr-info
      {
        qr-exists: true,
        is-active: (get active qr-info),
        not-expired: (< stacks-block-height (get expiry-block qr-info)),
        scans-available: (< (get current-scans qr-info) (get max-scans qr-info)),
        not-already-scanned: (is-none user-scan-data),
        daily-limit-ok: (< (get count daily-scans) MAX_DAILY_SCANS),
        cooldown-ok: (>= stacks-block-height (+ (get last-scan-block user-data) SCAN_COOLDOWN))
      }
      { qr-exists: false, is-active: false, not-expired: false, scans-available: false, not-already-scanned: false, daily-limit-ok: false, cooldown-ok: false }
    )
  )
)

;; private functions

;; Initialize contract
(begin
  (try! (ft-mint? scanbit-token INITIAL_TOKEN_SUPPLY CONTRACT_OWNER))
  (var-set contract-balance INITIAL_TOKEN_SUPPLY)
)
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
(define-constant ERR_CAMPAIGN_NOT_FOUND (err u109))
(define-constant ERR_CAMPAIGN_INACTIVE (err u110))
(define-constant ERR_CAMPAIGN_FULL (err u111))
(define-constant ERR_QR_NOT_IN_CAMPAIGN (err u112))
(define-constant ERR_INVALID_CAMPAIGN_PARAMS (err u113))

(define-constant INITIAL_TOKEN_SUPPLY u1000000000000)
(define-constant MAX_DAILY_SCANS u10)
(define-constant SCAN_COOLDOWN u144)
(define-constant BLOCKS_PER_DAY u144)

;; data vars
(define-data-var next-qr-id uint u1)
(define-data-var total-scans uint u0)
(define-data-var contract-balance uint u0)
(define-data-var next-campaign-id uint u1)

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

(define-map campaigns
  { campaign-id: uint }
  {
    creator: principal,
    name: (string-ascii 100),
    description: (string-ascii 500),
    total-qr-codes: uint,
    completion-bonus: uint,
    active: bool,
    max-participants: uint,
    current-participants: uint,
    expiry-block: uint,
    min-scans-for-bonus: uint
  }
)

(define-map campaign-qr-codes
  { campaign-id: uint, qr-sequence: uint }
  { qr-id: uint }
)

(define-map user-campaign-progress
  { user: principal, campaign-id: uint }
  {
    qr-codes-scanned: uint,
    completed: bool,
    bonus-claimed: bool,
    last-scan-block: uint,
    total-earned: uint
  }
)

(define-map campaign-participants
  { campaign-id: uint, participant-number: uint }
  { participant: principal }
)

(define-map campaign-leaderboard
  { campaign-id: uint, user: principal }
  {
    score: uint,
    completion-time: uint,
    rank: uint
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

(define-public (create-campaign (name (string-ascii 100)) (description (string-ascii 500)) (completion-bonus uint) (max-participants uint) (duration-blocks uint) (min-scans-for-bonus uint))
  (let
    (
      (campaign-id (var-get next-campaign-id))
      (expiry-block (+ stacks-block-height duration-blocks))
    )
    (asserts! (> completion-bonus u0) ERR_INVALID_CAMPAIGN_PARAMS)
    (asserts! (> max-participants u0) ERR_INVALID_CAMPAIGN_PARAMS)
    (asserts! (> duration-blocks u0) ERR_INVALID_CAMPAIGN_PARAMS)
    (asserts! (> min-scans-for-bonus u0) ERR_INVALID_CAMPAIGN_PARAMS)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      {
        creator: tx-sender,
        name: name,
        description: description,
        total-qr-codes: u0,
        completion-bonus: completion-bonus,
        active: true,
        max-participants: max-participants,
        current-participants: u0,
        expiry-block: expiry-block,
        min-scans-for-bonus: min-scans-for-bonus
      }
    )
    
    (var-set next-campaign-id (+ campaign-id u1))
    (ok campaign-id)
  )
)

(define-public (add-qr-to-campaign (campaign-id uint) (qr-id uint))
  (let
    (
      (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator campaign-data)) ERR_NOT_AUTHORIZED)
    (asserts! (get active campaign-data) ERR_CAMPAIGN_INACTIVE)
    
    (map-set campaign-qr-codes
      { campaign-id: campaign-id, qr-sequence: (get total-qr-codes campaign-data) }
      { qr-id: qr-id }
    )
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign-data { total-qr-codes: (+ (get total-qr-codes campaign-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (join-campaign (campaign-id uint))
  (let
    (
      (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (existing-progress (map-get? user-campaign-progress { user: tx-sender, campaign-id: campaign-id }))
    )
    (asserts! (get active campaign-data) ERR_CAMPAIGN_INACTIVE)
    (asserts! (< stacks-block-height (get expiry-block campaign-data)) ERR_QR_EXPIRED)
    (asserts! (< (get current-participants campaign-data) (get max-participants campaign-data)) ERR_CAMPAIGN_FULL)
    (asserts! (is-none existing-progress) ERR_QR_ALREADY_SCANNED)
    
    (map-set user-campaign-progress
      { user: tx-sender, campaign-id: campaign-id }
      {
        qr-codes-scanned: u0,
        completed: false,
        bonus-claimed: false,
        last-scan-block: u0,
        total-earned: u0
      }
    )
    
    (map-set campaign-participants
      { campaign-id: campaign-id, participant-number: (get current-participants campaign-data) }
      { participant: tx-sender }
    )
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign-data { current-participants: (+ (get current-participants campaign-data) u1) })
    )
    
    (ok true)
  )
)

(define-public (scan-campaign-qr (campaign-id uint) (qr-id uint))
  (let
    (
      (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
      (user-progress (unwrap! (map-get? user-campaign-progress { user: tx-sender, campaign-id: campaign-id }) ERR_NOT_AUTHORIZED))
      (user-scan-data (map-get? user-scans { user: tx-sender, qr-id: qr-id }))
      (qr-in-campaign (is-qr-in-campaign campaign-id qr-id))
    )
    (asserts! (get active campaign-data) ERR_CAMPAIGN_INACTIVE)
    (asserts! (< stacks-block-height (get expiry-block campaign-data)) ERR_QR_EXPIRED)
    (asserts! qr-in-campaign ERR_QR_NOT_IN_CAMPAIGN)
    (asserts! (is-none user-scan-data) ERR_QR_ALREADY_SCANNED)
    
    (try! (scan-qr-code qr-id))
    
    (let
      (
        (new-scanned-count (+ (get qr-codes-scanned user-progress) u1))
        (campaign-completed (>= new-scanned-count (get min-scans-for-bonus campaign-data)))
      )
      (map-set user-campaign-progress
        { user: tx-sender, campaign-id: campaign-id }
        (merge user-progress 
          {
            qr-codes-scanned: new-scanned-count,
            completed: campaign-completed,
            last-scan-block: stacks-block-height,
            total-earned: (+ (get total-earned user-progress) (get reward-amount qr-data))
          }
        )
      )
      
      (if campaign-completed
        (begin
          (map-set campaign-leaderboard
            { campaign-id: campaign-id, user: tx-sender }
            {
              score: new-scanned-count,
              completion-time: stacks-block-height,
              rank: u0
            }
          )
          (ok { scanned: true, completed: true, bonus-eligible: true })
        )
        (ok { scanned: true, completed: false, bonus-eligible: false })
      )
    )
  )
)

(define-public (claim-campaign-bonus (campaign-id uint))
  (let
    (
      (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
      (user-progress (unwrap! (map-get? user-campaign-progress { user: tx-sender, campaign-id: campaign-id }) ERR_NOT_AUTHORIZED))
    )
    (asserts! (get completed user-progress) ERR_NOT_AUTHORIZED)
    (asserts! (not (get bonus-claimed user-progress)) ERR_QR_ALREADY_SCANNED)
    (asserts! (>= (get qr-codes-scanned user-progress) (get min-scans-for-bonus campaign-data)) ERR_NOT_AUTHORIZED)
    
    (try! (ft-mint? scanbit-token (get completion-bonus campaign-data) tx-sender))
    
    (map-set user-campaign-progress
      { user: tx-sender, campaign-id: campaign-id }
      (merge user-progress { bonus-claimed: true })
    )
    
    (ok (get completion-bonus campaign-data))
  )
)

(define-public (deactivate-campaign (campaign-id uint))
  (let
    (
      (campaign-data (unwrap! (map-get? campaigns { campaign-id: campaign-id }) ERR_CAMPAIGN_NOT_FOUND))
    )
    (asserts! (is-eq tx-sender (get creator campaign-data)) ERR_NOT_AUTHORIZED)
    
    (map-set campaigns
      { campaign-id: campaign-id }
      (merge campaign-data { active: false })
    )
    (ok true)
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

(define-read-only (get-campaign (campaign-id uint))
  (map-get? campaigns { campaign-id: campaign-id })
)

(define-read-only (get-campaign-qr (campaign-id uint) (qr-sequence uint))
  (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: qr-sequence })
)

(define-read-only (get-user-campaign-progress (user principal) (campaign-id uint))
  (map-get? user-campaign-progress { user: user, campaign-id: campaign-id })
)

(define-read-only (get-campaign-participant (campaign-id uint) (participant-number uint))
  (map-get? campaign-participants { campaign-id: campaign-id, participant-number: participant-number })
)

(define-read-only (get-campaign-leaderboard (campaign-id uint) (user principal))
  (map-get? campaign-leaderboard { campaign-id: campaign-id, user: user })
)

(define-read-only (is-qr-in-campaign (campaign-id uint) (qr-id uint))
  (let
    (
      (campaign-data (map-get? campaigns { campaign-id: campaign-id }))
    )
    (match campaign-data
      campaign-info
      (let
        (
          (qr-0 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u0 }))
          (qr-1 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u1 }))
          (qr-2 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u2 }))
          (qr-3 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u3 }))
          (qr-4 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u4 }))
          (qr-5 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u5 }))
          (qr-6 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u6 }))
          (qr-7 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u7 }))
          (qr-8 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u8 }))
          (qr-9 (map-get? campaign-qr-codes { campaign-id: campaign-id, qr-sequence: u9 }))
        )
        (or
          (match qr-0 data-0 (is-eq (get qr-id data-0) qr-id) false)
          (match qr-1 data-1 (is-eq (get qr-id data-1) qr-id) false)
          (match qr-2 data-2 (is-eq (get qr-id data-2) qr-id) false)
          (match qr-3 data-3 (is-eq (get qr-id data-3) qr-id) false)
          (match qr-4 data-4 (is-eq (get qr-id data-4) qr-id) false)
          (match qr-5 data-5 (is-eq (get qr-id data-5) qr-id) false)
          (match qr-6 data-6 (is-eq (get qr-id data-6) qr-id) false)
          (match qr-7 data-7 (is-eq (get qr-id data-7) qr-id) false)
          (match qr-8 data-8 (is-eq (get qr-id data-8) qr-id) false)
          (match qr-9 data-9 (is-eq (get qr-id data-9) qr-id) false)
        )
      )
      false
    )
  )
)

(define-read-only (get-campaign-stats (campaign-id uint))
  (let
    (
      (campaign-data (map-get? campaigns { campaign-id: campaign-id }))
    )
    (match campaign-data
      campaign-info
      {
        exists: true,
        active: (get active campaign-info),
        participants: (get current-participants campaign-info),
        max-participants: (get max-participants campaign-info),
        total-qr-codes: (get total-qr-codes campaign-info),
        completion-bonus: (get completion-bonus campaign-info),
        blocks-remaining: (if (> (get expiry-block campaign-info) stacks-block-height)
                           (- (get expiry-block campaign-info) stacks-block-height)
                           u0)
      }
      { exists: false, active: false, participants: u0, max-participants: u0, total-qr-codes: u0, completion-bonus: u0, blocks-remaining: u0 }
    )
  )
)

;; private functions

;; Initialize contract
(begin
  (try! (ft-mint? scanbit-token INITIAL_TOKEN_SUPPLY CONTRACT_OWNER))
  (var-set contract-balance INITIAL_TOKEN_SUPPLY)
)
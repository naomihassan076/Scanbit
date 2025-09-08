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
(define-constant ERR_INSUFFICIENT_STAKE (err u114))
(define-constant ERR_NO_STAKE_FOUND (err u115))
(define-constant ERR_STAKE_PERIOD_ACTIVE (err u116))
(define-constant ERR_STAKE_ALREADY_EXISTS (err u117))
(define-constant ERR_YIELD_NOT_READY (err u118))
(define-constant ERR_BOOST_NOT_FOUND (err u119))
(define-constant ERR_BOOST_ALREADY_ACTIVE (err u120))
(define-constant ERR_INVALID_BOOST_MULTIPLIER (err u121))
(define-constant ERR_BOOST_EXPIRED (err u122))

(define-constant INITIAL_TOKEN_SUPPLY u1000000000000)
(define-constant MAX_DAILY_SCANS u10)
(define-constant SCAN_COOLDOWN u144)
(define-constant BLOCKS_PER_DAY u144)
(define-constant MIN_STAKE_AMOUNT u1000)
(define-constant STAKE_PERIOD_BLOCKS u1440)
(define-constant YIELD_RATE_MULTIPLIER u100)

;; data vars
(define-data-var next-qr-id uint u1)
(define-data-var total-scans uint u0)
(define-data-var contract-balance uint u0)
(define-data-var next-campaign-id uint u1)
(define-data-var next-boost-index uint u0)

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

(define-map qr-stakes
  { qr-id: uint, staker: principal }
  {
    stake-amount: uint,
    stake-start-block: uint,
    yield-earned: uint,
    last-yield-claim: uint,
    total-scans-when-staked: uint
  }
)

(define-map qr-stake-pool
  { qr-id: uint }
  {
    total-staked: uint,
    total-stakers: uint,
    total-yield-distributed: uint,
    scans-since-last-distribution: uint
  }
)

(define-map staker-portfolio
  { staker: principal }
  {
    total-staked: uint,
    total-yield-earned: uint,
    active-stakes: uint,
    best-performing-qr: uint
  }
)

(define-map qr-boosts
  { qr-id: uint }
  {
    boost-multiplier: uint,
    boost-expiry: uint
  }
)

(define-map boost-history
  { qr-id: uint, boost-index: uint }
  {
    activated-at: uint,
    multiplier: uint
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
      (boost-multiplier (get-current-boost qr-id))
      (final-reward (* (get reward-amount qr-data) boost-multiplier))
    )
    
    (asserts! (get active qr-data) ERR_QR_INACTIVE)
    (asserts! (< stacks-block-height (get expiry-block qr-data)) ERR_QR_EXPIRED)
    (asserts! (< (get current-scans qr-data) (get max-scans qr-data)) ERR_QR_ALREADY_SCANNED)
    (asserts! (is-none user-scan-data) ERR_QR_ALREADY_SCANNED)
    (asserts! (< (get count daily-scans) MAX_DAILY_SCANS) ERR_DAILY_LIMIT_EXCEEDED)
    (asserts! (>= stacks-block-height (+ (get last-scan-block user-data) SCAN_COOLDOWN)) ERR_COOLDOWN_ACTIVE)
    
    (try! (ft-mint? scanbit-token final-reward tx-sender))
    
    (map-set user-scans
      { user: tx-sender, qr-id: qr-id }
      {
        scanned-at: stacks-block-height,
        reward-earned: final-reward
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
        total-rewards: (+ (get total-rewards user-data) final-reward),
        last-scan-block: stacks-block-height,
        reputation-score: (+ (get reputation-score user-data) u1)
      }
    )
    
    (map-set qr-scan-history
      { qr-id: qr-id, scan-number: (get current-scans qr-data) }
      {
        scanner: tx-sender,
        scanned-at: stacks-block-height,
        reward-amount: final-reward
      }
    )
    
    (var-set total-scans (+ (var-get total-scans) u1))
    
    ;; Update stake pool with new scan
    (update-stake-pool-on-scan qr-id)
    
    (ok final-reward)
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

;; Staking functions
(define-public (stake-on-qr (qr-id uint) (stake-amount uint))
  (let
    (
      (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
      (existing-stake (map-get? qr-stakes { qr-id: qr-id, staker: tx-sender }))
      (current-pool (default-to { total-staked: u0, total-stakers: u0, total-yield-distributed: u0, scans-since-last-distribution: u0 }
                                 (map-get? qr-stake-pool { qr-id: qr-id })))
      (user-portfolio (default-to { total-staked: u0, total-yield-earned: u0, active-stakes: u0, best-performing-qr: u0 }
                                   (map-get? staker-portfolio { staker: tx-sender })))
    )
    (asserts! (>= stake-amount MIN_STAKE_AMOUNT) ERR_INSUFFICIENT_STAKE)
    (asserts! (get active qr-data) ERR_QR_INACTIVE)
    (asserts! (< stacks-block-height (get expiry-block qr-data)) ERR_QR_EXPIRED)
    (asserts! (is-none existing-stake) ERR_STAKE_ALREADY_EXISTS)
    
    ;; Transfer tokens from user
    (try! (ft-transfer? scanbit-token stake-amount tx-sender (as-contract tx-sender)))
    
    ;; Create stake record
    (map-set qr-stakes
      { qr-id: qr-id, staker: tx-sender }
      {
        stake-amount: stake-amount,
        stake-start-block: stacks-block-height,
        yield-earned: u0,
        last-yield-claim: stacks-block-height,
        total-scans-when-staked: (get current-scans qr-data)
      }
    )
    
    ;; Update stake pool
    (map-set qr-stake-pool
      { qr-id: qr-id }
      {
        total-staked: (+ (get total-staked current-pool) stake-amount),
        total-stakers: (+ (get total-stakers current-pool) u1),
        total-yield-distributed: (get total-yield-distributed current-pool),
        scans-since-last-distribution: (get scans-since-last-distribution current-pool)
      }
    )
    
    ;; Update user portfolio
    (map-set staker-portfolio
      { staker: tx-sender }
      {
        total-staked: (+ (get total-staked user-portfolio) stake-amount),
        total-yield-earned: (get total-yield-earned user-portfolio),
        active-stakes: (+ (get active-stakes user-portfolio) u1),
        best-performing-qr: (get best-performing-qr user-portfolio)
      }
    )
    
    (ok stake-amount)
  )
)

(define-public (unstake-from-qr (qr-id uint))
  (let
    (
      (stake-data (unwrap! (map-get? qr-stakes { qr-id: qr-id, staker: tx-sender }) ERR_NO_STAKE_FOUND))
      (current-pool (unwrap! (map-get? qr-stake-pool { qr-id: qr-id }) ERR_NO_STAKE_FOUND))
      (user-portfolio (unwrap! (map-get? staker-portfolio { staker: tx-sender }) ERR_NO_STAKE_FOUND))
    )
    (asserts! (>= stacks-block-height (+ (get stake-start-block stake-data) STAKE_PERIOD_BLOCKS)) ERR_STAKE_PERIOD_ACTIVE)
    
    ;; Calculate and claim any pending yield first
    (let
      (
        (pending-yield (calculate-pending-yield qr-id tx-sender))
      )
      (if (> pending-yield u0)
        (try! (as-contract (ft-mint? scanbit-token pending-yield tx-sender)))
        true
      )
      
      ;; Return staked amount
      (try! (as-contract (ft-transfer? scanbit-token (get stake-amount stake-data) (as-contract tx-sender) tx-sender)))
      
      ;; Remove stake record
      (map-delete qr-stakes { qr-id: qr-id, staker: tx-sender })
      
      ;; Update stake pool
      (map-set qr-stake-pool
        { qr-id: qr-id }
        {
          total-staked: (- (get total-staked current-pool) (get stake-amount stake-data)),
          total-stakers: (- (get total-stakers current-pool) u1),
          total-yield-distributed: (get total-yield-distributed current-pool),
          scans-since-last-distribution: (get scans-since-last-distribution current-pool)
        }
      )
      
      ;; Update user portfolio
      (map-set staker-portfolio
        { staker: tx-sender }
        {
          total-staked: (- (get total-staked user-portfolio) (get stake-amount stake-data)),
          total-yield-earned: (+ (get total-yield-earned user-portfolio) pending-yield),
          active-stakes: (- (get active-stakes user-portfolio) u1),
          best-performing-qr: (get best-performing-qr user-portfolio)
        }
      )
      
      (ok (+ (get stake-amount stake-data) pending-yield))
    )
  )
)

(define-public (claim-staking-yield (qr-id uint))
  (let
    (
      (stake-data (unwrap! (map-get? qr-stakes { qr-id: qr-id, staker: tx-sender }) ERR_NO_STAKE_FOUND))
      (pending-yield (calculate-pending-yield qr-id tx-sender))
      (user-portfolio (unwrap! (map-get? staker-portfolio { staker: tx-sender }) ERR_NO_STAKE_FOUND))
    )
    (asserts! (> pending-yield u0) ERR_YIELD_NOT_READY)
    
    ;; Mint yield tokens
    (try! (as-contract (ft-mint? scanbit-token pending-yield tx-sender)))
    
    ;; Update stake record
    (map-set qr-stakes
      { qr-id: qr-id, staker: tx-sender }
      (merge stake-data 
        {
          yield-earned: (+ (get yield-earned stake-data) pending-yield),
          last-yield-claim: stacks-block-height
        }
      )
    )
    
    ;; Update user portfolio
    (map-set staker-portfolio
      { staker: tx-sender }
      (merge user-portfolio 
        {
          total-yield-earned: (+ (get total-yield-earned user-portfolio) pending-yield),
          best-performing-qr: (if (> pending-yield u0) qr-id (get best-performing-qr user-portfolio))
        }
      )
    )
    
    (ok pending-yield)
  )
)

;; Boost system functions
(define-public (activate-boost (qr-id uint) (multiplier uint) (duration uint))
  (let (
    (qr-data (unwrap! (map-get? qr-codes { qr-id: qr-id }) ERR_QR_NOT_FOUND))
    (existing-boost (map-get? qr-boosts { qr-id: qr-id }))
  )
    (asserts! (is-eq (get creator qr-data) tx-sender) ERR_NOT_AUTHORIZED)
    (asserts! (is-none existing-boost) ERR_BOOST_ALREADY_ACTIVE)
    (asserts! (>= multiplier u1) ERR_INVALID_BOOST_MULTIPLIER)
    (asserts! (> duration u0) ERR_INVALID_BOOST_MULTIPLIER)
    
    (map-set qr-boosts
      { qr-id: qr-id }
      {
        boost-multiplier: multiplier,
        boost-expiry: (+ stacks-block-height duration)
      }
    )
    
    ;; Record history
    (let (
      (history-index (var-get next-boost-index))
    )
      (map-set boost-history
        { qr-id: qr-id, boost-index: history-index }
        {
          activated-at: stacks-block-height,
          multiplier: multiplier
        }
      )
      (var-set next-boost-index (+ history-index u1))
    )
    (ok true)
  )
)

(define-public (deactivate-boost (qr-id uint))
  (let (
    (qr-boost (unwrap! (map-get? qr-boosts { qr-id: qr-id }) ERR_BOOST_NOT_FOUND))
  )
    (asserts! (is-eq tx-sender (get creator (unwrap-panic (map-get? qr-codes { qr-id: qr-id })))) ERR_NOT_AUTHORIZED)
    (map-delete qr-boosts { qr-id: qr-id })
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

(define-read-only (get-qr-stake (qr-id uint) (staker principal))
  (map-get? qr-stakes { qr-id: qr-id, staker: staker })
)

(define-read-only (get-qr-stake-pool (qr-id uint))
  (map-get? qr-stake-pool { qr-id: qr-id })
)

(define-read-only (get-staker-portfolio (staker principal))
  (map-get? staker-portfolio { staker: staker })
)

(define-read-only (calculate-pending-yield (qr-id uint) (staker principal))
  (let
    (
      (stake-data (map-get? qr-stakes { qr-id: qr-id, staker: staker }))
      (qr-data (map-get? qr-codes { qr-id: qr-id }))
      (pool-data (map-get? qr-stake-pool { qr-id: qr-id }))
    )
    (match stake-data
      stake-info
      (match qr-data
        qr-info
        (match pool-data
          pool-info
          (let
            (
              (scans-since-stake (- (get current-scans qr-info) (get total-scans-when-staked stake-info)))
              (stake-share (if (> (get total-staked pool-info) u0)
                            (/ (* (get stake-amount stake-info) u10000) (get total-staked pool-info))
                            u0))
              (yield-per-scan (/ (* (get reward-amount qr-info) YIELD_RATE_MULTIPLIER) u10000))
              (total-yield (/ (* scans-since-stake yield-per-scan stake-share) u10000))
            )
            total-yield
          )
          u0
        )
        u0
      )
      u0
    )
  )
)

(define-read-only (get-staking-stats (qr-id uint))
  (let
    (
      (qr-data (map-get? qr-codes { qr-id: qr-id }))
      (pool-data (map-get? qr-stake-pool { qr-id: qr-id }))
    )
    (match qr-data
      qr-info
      (match pool-data
        pool-info
        {
          exists: true,
          total-staked: (get total-staked pool-info),
          total-stakers: (get total-stakers pool-info),
          current-scans: (get current-scans qr-info),
          reward-amount: (get reward-amount qr-info),
          apy-estimate: (if (> (get total-staked pool-info) u0)
                         (/ (* (get reward-amount qr-info) YIELD_RATE_MULTIPLIER u365) (get total-staked pool-info))
                         u0)
        }
        { exists: false, total-staked: u0, total-stakers: u0, current-scans: u0, reward-amount: u0, apy-estimate: u0 }
      )
      { exists: false, total-staked: u0, total-stakers: u0, current-scans: u0, reward-amount: u0, apy-estimate: u0 }
    )
  )
)

;; Boost system read-only functions
(define-read-only (get-boost-info (qr-id uint))
  (let (
    (boost (map-get? qr-boosts { qr-id: qr-id }))
  )
    (match boost
      some-boost some-boost
      { boost-multiplier: u1, boost-expiry: u0 }
    )
  )
)

(define-read-only (get-boost-history (qr-id uint) (index uint))
  (map-get? boost-history { qr-id: qr-id, boost-index: index })
)

(define-read-only (get-current-boost (qr-id uint))
  (let (
    (boost (map-get? qr-boosts { qr-id: qr-id }))
  )
    (match boost
      some-boost
      (if (> (get boost-expiry some-boost) stacks-block-height)
        (get boost-multiplier some-boost)
        u1
      )
      u1
    )
  )
)

;; private functions

(define-private (update-stake-pool-on-scan (qr-id uint))
  (let
    (
      (current-pool (map-get? qr-stake-pool { qr-id: qr-id }))
    )
    (match current-pool
      pool-data
      (map-set qr-stake-pool
        { qr-id: qr-id }
        (merge pool-data { scans-since-last-distribution: (+ (get scans-since-last-distribution pool-data) u1) })
      )
      true
    )
  )
)

;; Initialize contract
(begin
  (try! (ft-mint? scanbit-token INITIAL_TOKEN_SUPPLY CONTRACT_OWNER))
  (var-set contract-balance INITIAL_TOKEN_SUPPLY)
)



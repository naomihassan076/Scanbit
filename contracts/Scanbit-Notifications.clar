;; title: Scanbit Notifications
;; version: 1.0.0
;; summary: QR Code Expiry Notification System
;; description: A notification system for tracking and alerting about expiring QR codes in the Scanbit ecosystem

;; constants
(define-constant CONTRACT_OWNER tx-sender)
(define-constant ERR_NOT_AUTHORIZED (err u200))
(define-constant ERR_INVALID_SUBSCRIPTION (err u201))
(define-constant ERR_SUBSCRIPTION_NOT_FOUND (err u202))
(define-constant ERR_QR_NOT_FOUND (err u203))
(define-constant ERR_INVALID_THRESHOLD (err u204))
(define-constant ERR_NOTIFICATION_ALREADY_SENT (err u205))
(define-constant ERR_USER_NOT_SUBSCRIBED (err u206))

;; notification thresholds in blocks
(define-constant THRESHOLD_24_HOURS u144)
(define-constant THRESHOLD_1_HOUR u6)
(define-constant THRESHOLD_10_MINUTES u1)
(define-constant MAX_NOTIFICATIONS_PER_QR u3)

;; data vars
(define-data-var notification-id-nonce uint u1)
(define-data-var total-subscriptions uint u0)

;; data maps
(define-map notification-subscriptions
  { user: principal }
  {
    subscribed: bool,
    threshold-preference: uint,
    notifications-enabled: bool,
    subscription-date: uint,
    last-notification-block: uint
  }
)

(define-map qr-expiry-alerts
  { qr-id: uint, notification-type: uint }
  {
    threshold-blocks: uint,
    alert-sent: bool,
    alert-sent-block: uint,
    subscribers-notified: uint
  }
)

(define-map user-qr-notifications
  { user: principal, qr-id: uint }
  {
    interested: bool,
    notification-sent: bool,
    marked-at-block: uint,
    expiry-block: uint
  }
)

(define-map expiry-notification-history
  { notification-id: uint }
  {
    qr-id: uint,
    notification-type: uint,
    sent-at-block: uint,
    recipients-count: uint,
    threshold-used: uint
  }
)

(define-map creator-notification-settings
  { creator: principal }
  {
    notify-on-expiry: bool,
    advance-warning-blocks: uint,
    email-notifications: bool,
    auto-extend-enabled: bool
  }
)

;; public functions

(define-public (subscribe-to-notifications (threshold-preference uint))
  (begin
    (asserts! (or (is-eq threshold-preference THRESHOLD_24_HOURS)
                  (is-eq threshold-preference THRESHOLD_1_HOUR)
                  (is-eq threshold-preference THRESHOLD_10_MINUTES)) ERR_INVALID_THRESHOLD)
    
    (map-set notification-subscriptions
      { user: tx-sender }
      {
        subscribed: true,
        threshold-preference: threshold-preference,
        notifications-enabled: true,
        subscription-date: stacks-block-height,
        last-notification-block: u0
      }
    )
    
    (var-set total-subscriptions (+ (var-get total-subscriptions) u1))
    (ok true)
  )
)

(define-public (unsubscribe-from-notifications)
  (let
    (
      (subscription (unwrap! (map-get? notification-subscriptions { user: tx-sender }) ERR_SUBSCRIPTION_NOT_FOUND))
    )
    (map-set notification-subscriptions
      { user: tx-sender }
      (merge subscription { subscribed: false, notifications-enabled: false })
    )
    (ok true)
  )
)

(define-public (mark-qr-of-interest (qr-id uint))
  (let
    (
      (qr-data (unwrap! (contract-call? .Scanbit get-qr-code qr-id) ERR_QR_NOT_FOUND))
    )
    (map-set user-qr-notifications
      { user: tx-sender, qr-id: qr-id }
      {
        interested: true,
        notification-sent: false,
        marked-at-block: stacks-block-height,
        expiry-block: (get expiry-block qr-data)
      }
    )
    (ok true)
  )
)

(define-public (unmark-qr-of-interest (qr-id uint))
  (let
    (
      (existing-interest (unwrap! (map-get? user-qr-notifications { user: tx-sender, qr-id: qr-id }) ERR_INVALID_SUBSCRIPTION))
    )
    (map-set user-qr-notifications
      { user: tx-sender, qr-id: qr-id }
      (merge existing-interest { interested: false })
    )
    (ok true)
  )
)

(define-public (set-creator-notification-preferences (notify-on-expiry bool) (advance-warning-blocks uint) (email-notifications bool) (auto-extend-enabled bool))
  (begin
    (asserts! (<= advance-warning-blocks u1440) ERR_INVALID_THRESHOLD) ;; max 10 days
    (asserts! (>= advance-warning-blocks u1) ERR_INVALID_THRESHOLD)     ;; min 10 minutes
    
    (map-set creator-notification-settings
      { creator: tx-sender }
      {
        notify-on-expiry: notify-on-expiry,
        advance-warning-blocks: advance-warning-blocks,
        email-notifications: email-notifications,
        auto-extend-enabled: auto-extend-enabled
      }
    )
    (ok true)
  )
)

(define-public (process-expiry-notifications (qr-id uint))
  (let
    (
      (qr-data (unwrap! (contract-call? .Scanbit get-qr-code qr-id) ERR_QR_NOT_FOUND))
      (expiry-block (get expiry-block qr-data))
      (blocks-until-expiry (if (> expiry-block stacks-block-height) 
                            (- expiry-block stacks-block-height) 
                            u0))
      (notification-id (var-get notification-id-nonce))
    )
    
    ;; Check if we should send 24-hour notification
    (if (and (<= blocks-until-expiry THRESHOLD_24_HOURS) (> blocks-until-expiry THRESHOLD_1_HOUR))
      (begin
        (try! (send-expiry-notification qr-id u1 THRESHOLD_24_HOURS))
        (var-set notification-id-nonce (+ notification-id u1))
      )
      true
    )
    
    ;; Check if we should send 1-hour notification  
    (if (and (<= blocks-until-expiry THRESHOLD_1_HOUR) (> blocks-until-expiry THRESHOLD_10_MINUTES))
      (begin
        (try! (send-expiry-notification qr-id u2 THRESHOLD_1_HOUR))
        (var-set notification-id-nonce (+ notification-id u1))
      )
      true
    )
    
    ;; Check if we should send final 10-minute notification
    (if (<= blocks-until-expiry THRESHOLD_10_MINUTES)
      (begin
        (try! (send-expiry-notification qr-id u3 THRESHOLD_10_MINUTES))
        (var-set notification-id-nonce (+ notification-id u1))
      )
      true
    )
    
    (ok blocks-until-expiry)
  )
)

;; read-only functions

(define-read-only (get-user-subscription (user principal))
  (map-get? notification-subscriptions { user: user })
)

(define-read-only (get-qr-notification-status (user principal) (qr-id uint))
  (map-get? user-qr-notifications { user: user, qr-id: qr-id })
)

(define-read-only (get-expiry-alert-status (qr-id uint) (notification-type uint))
  (map-get? qr-expiry-alerts { qr-id: qr-id, notification-type: notification-type })
)

(define-read-only (get-notification-history (notification-id uint))
  (map-get? expiry-notification-history { notification-id: notification-id })
)

(define-read-only (get-creator-settings (creator principal))
  (map-get? creator-notification-settings { creator: creator })
)

(define-read-only (get-expiring-qrs (threshold-blocks uint))
  (let
    (
      (current-block stacks-block-height)
    )
    ;; This would typically require iteration over QR codes
    ;; For simplicity, returning a structure that indicates what to check
    {
      current-block: current-block,
      threshold: threshold-blocks,
      check-block: (+ current-block threshold-blocks)
    }
  )
)

(define-read-only (check-qr-expiry-status (qr-id uint))
  (let
    (
      (qr-data (contract-call? .Scanbit get-qr-code qr-id))
    )
    (match qr-data
      qr-info
      (let
        (
          (expiry-block (get expiry-block qr-info))
          (blocks-until-expiry (if (> expiry-block stacks-block-height)
                                (- expiry-block stacks-block-height)
                                u0))
        )
        {
          qr-exists: true,
          expiry-block: expiry-block,
          blocks-until-expiry: blocks-until-expiry,
          expired: (>= stacks-block-height expiry-block),
          expires-in-24h: (<= blocks-until-expiry THRESHOLD_24_HOURS),
          expires-in-1h: (<= blocks-until-expiry THRESHOLD_1_HOUR),
          expires-in-10m: (<= blocks-until-expiry THRESHOLD_10_MINUTES)
        }
      )
      {
        qr-exists: false,
        expiry-block: u0,
        blocks-until-expiry: u0,
        expired: false,
        expires-in-24h: false,
        expires-in-1h: false,
        expires-in-10m: false
      }
    )
  )
)

(define-read-only (get-user-interested-qrs-count (user principal))
  ;; This is a simplified version - in practice, you'd iterate through user interests
  (let
    (
      (subscription (map-get? notification-subscriptions { user: user }))
    )
    (match subscription
      sub-data
      {
        has-subscription: true,
        is-subscribed: (get subscribed sub-data),
        notifications-enabled: (get notifications-enabled sub-data)
      }
      {
        has-subscription: false,
        is-subscribed: false,
        notifications-enabled: false
      }
    )
  )
)

(define-read-only (get-notification-stats)
  {
    total-subscriptions: (var-get total-subscriptions),
    current-notification-id: (var-get notification-id-nonce),
    current-block: stacks-block-height,
    threshold-24h: THRESHOLD_24_HOURS,
    threshold-1h: THRESHOLD_1_HOUR,
    threshold-10m: THRESHOLD_10_MINUTES
  }
)

;; private functions

(define-private (send-expiry-notification (qr-id uint) (notification-type uint) (threshold uint))
  (let
    (
      (existing-alert (map-get? qr-expiry-alerts { qr-id: qr-id, notification-type: notification-type }))
      (notification-id (var-get notification-id-nonce))
    )
    ;; Only send if not already sent
    (asserts! (match existing-alert
                alert-data (not (get alert-sent alert-data))
                true) ERR_NOTIFICATION_ALREADY_SENT)
    
    ;; Record the alert
    (map-set qr-expiry-alerts
      { qr-id: qr-id, notification-type: notification-type }
      {
        threshold-blocks: threshold,
        alert-sent: true,
        alert-sent-block: stacks-block-height,
        subscribers-notified: u1 ;; simplified - would calculate actual subscribers
      }
    )
    
    ;; Record in history
    (map-set expiry-notification-history
      { notification-id: notification-id }
      {
        qr-id: qr-id,
        notification-type: notification-type,
        sent-at-block: stacks-block-height,
        recipients-count: u1,
        threshold-used: threshold
      }
    )
    
    (ok true)
  )
)

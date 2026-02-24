;; ---------------------------------------------------------
;; Contract: CrowdFundX
;; Purpose: Decentralized Community Crowdfunding Vault
;; ---------------------------------------------------------

(define-constant ERR_CAMPAIGN_NOT_FOUND (err u100))
(define-constant ERR_DEADLINE_PASSED (err u102))
(define-constant ERR_GOAL_NOT_REACHED (err u103))
(define-constant ERR_NOT_CREATOR (err u104))
(define-constant ERR_NOTHING_TO_REFUND (err u105))

;; ----------------------------
;; Campaign Structure
;; ----------------------------
(define-map campaigns
  { id: uint }
  {
    creator: principal,
    goal: uint,
    deadline: uint,
    raised: uint,
    successful: bool
  })

;; Tracks contributions: (campaign-id, user) -> amount
(define-map contributions
  { id: uint, user: principal }
  { amount: uint })

(define-data-var next-campaign-id uint u0)

;; ----------------------------
;; Create a new campaign
;; ----------------------------
(define-public (create-campaign (goal uint) (duration uint))
  (begin
    (asserts! (> goal u0) ERR_GOAL_NOT_REACHED)
    (asserts! (> duration u0) ERR_GOAL_NOT_REACHED)
    (let ((id (var-get next-campaign-id)))
      (map-set campaigns { id: id }
      {
        creator: tx-sender,
        goal: goal,
        deadline: (+ stacks-block-height duration),
        raised: u0,
        successful: false
      })
      (var-set next-campaign-id (+ id u1))
      (ok id))))

;; ----------------------------
;; Contribute STX to a campaign
;; ----------------------------
(define-public (contribute (id uint) (amount uint))
  (begin
    (asserts! (> amount u0) ERR_GOAL_NOT_REACHED)
    (let ((checked-id id))
      (match (map-get? campaigns { id: checked-id })
        campaign (if (>= stacks-block-height (get deadline campaign))
          ERR_DEADLINE_PASSED
          (let ((contrib (map-get? contributions { id: checked-id, user: tx-sender })))
            (let ((existing-contribution (match contrib v (get amount v) u0)))
              (let ((new-amount (+ amount existing-contribution))
                    (new-raised (+ (get raised campaign) amount)))
                (begin
                  (map-set contributions { id: checked-id, user: tx-sender } 
                    { amount: new-amount })
                  (map-set campaigns { id: checked-id }
                    (merge campaign 
                      { raised: new-raised }))
                  (ok true))))))
        ERR_CAMPAIGN_NOT_FOUND))))

;; ----------------------------
;; Finalize campaign (mark as successful if goal met)
;; ----------------------------
(define-public (finalize-campaign (id uint))
  (let ((checked-id id))
    (asserts! (is-some (map-get? campaigns { id: checked-id })) ERR_CAMPAIGN_NOT_FOUND)
    (match (map-get? campaigns { id: checked-id })
      campaign
        (if (>= (get raised campaign) (get goal campaign))
            (begin
              (map-set campaigns { id: checked-id }
                {
                  creator: (get creator campaign),
                  goal: (get goal campaign),
                  deadline: (get deadline campaign),
                  raised: (get raised campaign),
                  successful: true
                })
              (ok true))
            ERR_GOAL_NOT_REACHED)
      ERR_CAMPAIGN_NOT_FOUND)))

;; ----------------------------
;; Withdraw funds (only creator if successful)
;; ----------------------------
(define-public (withdraw (id uint))
  (let ((checked-id id))
    (asserts! (is-some (map-get? campaigns { id: checked-id })) ERR_CAMPAIGN_NOT_FOUND)
    (match (map-get? campaigns { id: checked-id })
      campaign
        (if (and (is-eq tx-sender (get creator campaign)) (get successful campaign))
            (let ((amount (get raised campaign)))
              (map-set campaigns { id: checked-id } (merge campaign { raised: u0 }))
              (stx-transfer? amount (as-contract tx-sender) tx-sender))
            ERR_NOT_CREATOR)
      ERR_CAMPAIGN_NOT_FOUND)))

;; ----------------------------
;; Refund supporter if campaign failed
;; ----------------------------
(define-public (refund (id uint))
  (let ((checked-id id))
    (asserts! (is-some (map-get? campaigns { id: checked-id })) ERR_CAMPAIGN_NOT_FOUND)
    (match (map-get? campaigns { id: checked-id })
      campaign
        ;; Fixed block-height to stacks-block-height for consistency
        (if (and (>= stacks-block-height (get deadline campaign)) (not (get successful campaign)))
          (match (map-get? contributions { id: checked-id, user: tx-sender })
            contrib
              (let ((amt (get amount contrib)))
                (if (> amt u0)
                  (begin
                    (map-delete contributions { id: checked-id, user: tx-sender })
                    (stx-transfer? amt (as-contract tx-sender) tx-sender))
                  ERR_NOTHING_TO_REFUND))
            ERR_NOTHING_TO_REFUND)
          ERR_GOAL_NOT_REACHED)
      ERR_CAMPAIGN_NOT_FOUND)))

;; ----------------------------
;; View Functions
;; ----------------------------
(define-read-only (get-campaign (id uint))
  (map-get? campaigns { id: id }))

(define-read-only (get-contribution (id uint) (user principal))
  (map-get? contributions { id: id, user: user }))

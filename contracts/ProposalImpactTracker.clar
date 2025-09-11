;; Proposal Impact Tracker for Decentralized Referendum System
;; Tracks real-world impact and community feedback on passed referendum proposals

(define-constant CONTRACT-OWNER tx-sender)

;; Error constants
(define-constant ERR-UNAUTHORIZED (err u400))
(define-constant ERR-NOT-FOUND (err u401))
(define-constant ERR-INVALID-STATE (err u402))
(define-constant ERR-INVALID-RATING (err u403))
(define-constant ERR-ALREADY-EXISTS (err u404))
(define-constant ERR-INVALID-TIMEFRAME (err u405))
(define-constant ERR-INVALID-EVIDENCE (err u406))
(define-constant ERR-DUPLICATE-FEEDBACK (err u407))

;; Constants for validation
(define-constant MIN-IMPACT-RATING u1)
(define-constant MAX-IMPACT-RATING u10)
(define-constant MIN-FEEDBACK-LENGTH u10)
(define-constant MAX-FEEDBACK-LENGTH u500)
(define-constant IMPACT-REVIEW-PERIOD u4320) ;; ~30 days in blocks

;; Proposal impact tracking data
(define-map proposal-impacts
  { proposal-id: uint }
  {
    implementation-status: (string-ascii 20), ;; "pending", "in-progress", "completed", "failed"
    expected-completion: uint, ;; Block height
    actual-completion: (optional uint), ;; Block height when completed
    implementation-cost: uint, ;; Actual cost vs expected
    expected-cost: uint, ;; Original estimated cost
    impact-score: uint, ;; Community-rated impact (1-10)
    effectiveness-score: uint, ;; How well it achieved goals (1-10)
    total-feedback-count: uint,
    average-community-rating: uint,
    is-measurable: bool, ;; Can the impact be objectively measured
    measurement-criteria: (optional (string-ascii 200)),
    last-updated: uint
  }
)

;; Community feedback on proposal impacts
(define-map impact-feedback
  { proposal-id: uint, feedback-id: uint }
  {
    submitter: principal,
    feedback-text: (string-ascii 500),
    impact-rating: uint, ;; 1-10 scale
    effectiveness-rating: uint, ;; 1-10 scale
    evidence-provided: bool,
    evidence-hash: (optional (buff 32)),
    feedback-date: uint,
    is-verified: bool, ;; Admin can verify credible feedback
    upvotes: uint,
    downvotes: uint
  }
)

;; Track next feedback ID for each proposal
(define-map proposal-feedback-counters
  { proposal-id: uint }
  { next-feedback-id: uint }
)

;; Evidence submissions for proposal impacts
(define-map impact-evidence
  { proposal-id: uint, evidence-id: uint }
  {
    submitter: principal,
    evidence-type: (string-ascii 30), ;; "document", "data", "report", "photo", "video"
    evidence-hash: (buff 32),
    evidence-description: (string-ascii 200),
    submission-date: uint,
    verification-status: (string-ascii 20), ;; "pending", "verified", "rejected"
    verified-by: (optional principal)
  }
)

;; Track evidence counters
(define-map evidence-counters
  { proposal-id: uint }
  { next-evidence-id: uint }
)

;; User engagement tracking for impact assessment
(define-map user-impact-activity
  { user: principal }
  {
    total-feedback-submitted: uint,
    total-evidence-provided: uint,
    verified-contributions: uint,
    reputation-score: uint,
    last-activity: uint
  }
)

;; Impact milestones for long-term tracking
(define-map impact-milestones
  { proposal-id: uint, milestone-id: uint }
  {
    milestone-description: (string-ascii 200),
    target-date: uint,
    completion-date: (optional uint),
    is-achieved: bool,
    achievement-evidence: (optional (buff 32)),
    community-verification: uint ;; Number of users who verified this milestone
  }
)

;; Helper functions

;; Check if user is admin
(define-private (is-admin (user principal))
  (is-eq user CONTRACT-OWNER)
)

;; Validate impact rating
(define-private (is-valid-rating (rating uint))
  (and (>= rating MIN-IMPACT-RATING) (<= rating MAX-IMPACT-RATING))
)

;; Calculate average rating
(define-private (calculate-average (total-score uint) (count uint))
  (if (> count u0)
    (/ total-score count)
    u0
  )
)

;; Public functions

;; Initialize impact tracking for a passed proposal
(define-public (initialize-proposal-impact 
  (proposal-id uint) 
  (expected-completion uint) 
  (expected-cost uint) 
  (is-measurable bool) 
  (measurement-criteria (optional (string-ascii 200))))
  (let ((existing-impact (map-get? proposal-impacts { proposal-id: proposal-id })))
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    (asserts! (is-none existing-impact) ERR-ALREADY-EXISTS)
    (asserts! (>= expected-completion stacks-block-height) ERR-INVALID-TIMEFRAME)
    
    (map-set proposal-impacts 
      { proposal-id: proposal-id }
      {
        implementation-status: "pending",
        expected-completion: expected-completion,
        actual-completion: none,
        implementation-cost: u0,
        expected-cost: expected-cost,
        impact-score: u0,
        effectiveness-score: u0,
        total-feedback-count: u0,
        average-community-rating: u0,
        is-measurable: is-measurable,
        measurement-criteria: measurement-criteria,
        last-updated: stacks-block-height
      }
    )
    
    ;; Initialize feedback counter
    (map-set proposal-feedback-counters 
      { proposal-id: proposal-id } 
      { next-feedback-id: u1 }
    )
    
    ;; Initialize evidence counter
    (map-set evidence-counters 
      { proposal-id: proposal-id } 
      { next-evidence-id: u1 }
    )
    
    (ok true)
  )
)

;; Update proposal implementation status
(define-public (update-implementation-status 
  (proposal-id uint) 
  (new-status (string-ascii 20)) 
  (actual-cost uint))
  (let ((impact-data (unwrap! (map-get? proposal-impacts { proposal-id: proposal-id }) ERR-NOT-FOUND)))
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    
    (map-set proposal-impacts 
      { proposal-id: proposal-id }
      (merge impact-data {
        implementation-status: new-status,
        implementation-cost: actual-cost,
        actual-completion: (if (or (is-eq new-status "completed") (is-eq new-status "failed"))
                             (some stacks-block-height)
                             (get actual-completion impact-data)),
        last-updated: stacks-block-height
      })
    )
    
    (ok true)
  )
)

;; Submit community feedback on proposal impact
(define-public (submit-impact-feedback 
  (proposal-id uint) 
  (feedback-text (string-ascii 500)) 
  (impact-rating uint) 
  (effectiveness-rating uint) 
  (evidence-hash (optional (buff 32))))
  (let (
    (impact-data (unwrap! (map-get? proposal-impacts { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (feedback-counter (unwrap! (map-get? proposal-feedback-counters { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (feedback-id (get next-feedback-id feedback-counter))
    (feedback-length (len feedback-text))
    (user-activity (default-to 
      { total-feedback-submitted: u0, total-evidence-provided: u0, verified-contributions: u0, reputation-score: u0, last-activity: u0 }
      (map-get? user-impact-activity { user: tx-sender })
    ))
  )
    ;; Validate inputs
    (asserts! (and (>= feedback-length MIN-FEEDBACK-LENGTH) (<= feedback-length MAX-FEEDBACK-LENGTH)) ERR-INVALID-EVIDENCE)
    (asserts! (is-valid-rating impact-rating) ERR-INVALID-RATING)
    (asserts! (is-valid-rating effectiveness-rating) ERR-INVALID-RATING)
    
    ;; Store feedback
    (map-set impact-feedback 
      { proposal-id: proposal-id, feedback-id: feedback-id }
      {
        submitter: tx-sender,
        feedback-text: feedback-text,
        impact-rating: impact-rating,
        effectiveness-rating: effectiveness-rating,
        evidence-provided: (is-some evidence-hash),
        evidence-hash: evidence-hash,
        feedback-date: stacks-block-height,
        is-verified: false,
        upvotes: u0,
        downvotes: u0
      }
    )
    
    ;; Update counters and averages
    (let (
      (new-feedback-count (+ (get total-feedback-count impact-data) u1))
      (total-impact-score (+ (* (get average-community-rating impact-data) (get total-feedback-count impact-data)) impact-rating))
      (new-average (calculate-average total-impact-score new-feedback-count))
    )
      (map-set proposal-impacts 
        { proposal-id: proposal-id }
        (merge impact-data {
          total-feedback-count: new-feedback-count,
          average-community-rating: new-average,
          last-updated: stacks-block-height
        })
      )
    )
    
    ;; Update feedback counter
    (map-set proposal-feedback-counters 
      { proposal-id: proposal-id } 
      { next-feedback-id: (+ feedback-id u1) }
    )
    
    ;; Update user activity
    (map-set user-impact-activity 
      { user: tx-sender }
      (merge user-activity {
        total-feedback-submitted: (+ (get total-feedback-submitted user-activity) u1),
        total-evidence-provided: (+ (get total-evidence-provided user-activity) (if (is-some evidence-hash) u1 u0)),
        last-activity: stacks-block-height
      })
    )
    
    (ok feedback-id)
  )
)

;; Submit evidence for proposal impact
(define-public (submit-impact-evidence 
  (proposal-id uint) 
  (evidence-type (string-ascii 30)) 
  (evidence-hash (buff 32)) 
  (evidence-description (string-ascii 200)))
  (let (
    (evidence-counter (unwrap! (map-get? evidence-counters { proposal-id: proposal-id }) ERR-NOT-FOUND))
    (evidence-id (get next-evidence-id evidence-counter))
    (user-activity (default-to 
      { total-feedback-submitted: u0, total-evidence-provided: u0, verified-contributions: u0, reputation-score: u0, last-activity: u0 }
      (map-get? user-impact-activity { user: tx-sender })
    ))
  )
    (asserts! (> (len evidence-hash) u0) ERR-INVALID-EVIDENCE)
    (asserts! (> (len evidence-description) u0) ERR-INVALID-EVIDENCE)
    
    (map-set impact-evidence 
      { proposal-id: proposal-id, evidence-id: evidence-id }
      {
        submitter: tx-sender,
        evidence-type: evidence-type,
        evidence-hash: evidence-hash,
        evidence-description: evidence-description,
        submission-date: stacks-block-height,
        verification-status: "pending",
        verified-by: none
      }
    )
    
    ;; Update evidence counter
    (map-set evidence-counters 
      { proposal-id: proposal-id } 
      { next-evidence-id: (+ evidence-id u1) }
    )
    
    ;; Update user activity
    (map-set user-impact-activity 
      { user: tx-sender }
      (merge user-activity {
        total-evidence-provided: (+ (get total-evidence-provided user-activity) u1),
        last-activity: stacks-block-height
      })
    )
    
    (ok evidence-id)
  )
)

;; Verify evidence submission (admin only)
(define-public (verify-evidence 
  (proposal-id uint) 
  (evidence-id uint) 
  (verification-status (string-ascii 20)))
  (let ((evidence-data (unwrap! (map-get? impact-evidence { proposal-id: proposal-id, evidence-id: evidence-id }) ERR-NOT-FOUND)))
    (asserts! (is-admin tx-sender) ERR-UNAUTHORIZED)
    
    (map-set impact-evidence 
      { proposal-id: proposal-id, evidence-id: evidence-id }
      (merge evidence-data {
        verification-status: verification-status,
        verified-by: (some tx-sender)
      })
    )
    
    ;; Update user reputation if verified
    (if (is-eq verification-status "verified")
      (let ((submitter (get submitter evidence-data))
            (user-activity (default-to 
              { total-feedback-submitted: u0, total-evidence-provided: u0, verified-contributions: u0, reputation-score: u0, last-activity: u0 }
              (map-get? user-impact-activity { user: submitter })
            )))
        (map-set user-impact-activity 
          { user: submitter }
          (merge user-activity {
            verified-contributions: (+ (get verified-contributions user-activity) u1),
            reputation-score: (+ (get reputation-score user-activity) u10)
          })
        )
      )
      true
    )
    
    (ok true)
  )
)

;; Vote on feedback quality (upvote/downvote)
(define-public (vote-on-feedback 
  (proposal-id uint) 
  (feedback-id uint) 
  (is-upvote bool))
  (let ((feedback-data (unwrap! (map-get? impact-feedback { proposal-id: proposal-id, feedback-id: feedback-id }) ERR-NOT-FOUND)))
    (asserts! (not (is-eq tx-sender (get submitter feedback-data))) ERR-UNAUTHORIZED)
    
    (map-set impact-feedback 
      { proposal-id: proposal-id, feedback-id: feedback-id }
      (merge feedback-data {
        upvotes: (if is-upvote (+ (get upvotes feedback-data) u1) (get upvotes feedback-data)),
        downvotes: (if is-upvote (get downvotes feedback-data) (+ (get downvotes feedback-data) u1))
      })
    )
    
    (ok true)
  )
)

;; Read-only functions

;; Get proposal impact data
(define-read-only (get-proposal-impact (proposal-id uint))
  (map-get? proposal-impacts { proposal-id: proposal-id })
)

;; Get feedback for a proposal
(define-read-only (get-proposal-feedback (proposal-id uint) (feedback-id uint))
  (map-get? impact-feedback { proposal-id: proposal-id, feedback-id: feedback-id })
)

;; Get evidence for a proposal
(define-read-only (get-proposal-evidence (proposal-id uint) (evidence-id uint))
  (map-get? impact-evidence { proposal-id: proposal-id, evidence-id: evidence-id })
)

;; Get user impact activity
(define-read-only (get-user-impact-activity (user principal))
  (default-to 
    { total-feedback-submitted: u0, total-evidence-provided: u0, verified-contributions: u0, reputation-score: u0, last-activity: u0 }
    (map-get? user-impact-activity { user: user })
  )
)

;; Get proposal feedback count
(define-read-only (get-proposal-feedback-count (proposal-id uint))
  (default-to { next-feedback-id: u1 } (map-get? proposal-feedback-counters { proposal-id: proposal-id }))
)

;; Get proposal evidence count
(define-read-only (get-proposal-evidence-count (proposal-id uint))
  (default-to { next-evidence-id: u1 } (map-get? evidence-counters { proposal-id: proposal-id }))
)

;; Calculate impact effectiveness score
(define-read-only (calculate-impact-effectiveness (proposal-id uint))
  (match (map-get? proposal-impacts { proposal-id: proposal-id })
    impact-data
    (let (
      (implementation-score (if (is-eq (get implementation-status impact-data) "completed")
        u100
        (if (is-eq (get implementation-status impact-data) "in-progress")
          u50
          (if (is-eq (get implementation-status impact-data) "failed")
            u0
            u25 ;; pending
          )
        )
      ))
      (cost-efficiency (if (> (get expected-cost impact-data) u0)
        (let ((actual-cost (get implementation-cost impact-data)))
          (if (<= actual-cost (get expected-cost impact-data))
            u100
(let ((cost-penalty (/ (* (- actual-cost (get expected-cost impact-data)) u100) (get expected-cost impact-data))))
              (if (> cost-penalty u100) u0 (- u100 cost-penalty))
            )
          )
        )
        u100
      ))
      (community-score (* (get average-community-rating impact-data) u10))
      (timeliness-score (match (get actual-completion impact-data)
        completion-block
        (if (<= completion-block (get expected-completion impact-data))
          u100
(let ((time-penalty (/ (* (- completion-block (get expected-completion impact-data)) u100) IMPACT-REVIEW-PERIOD)))
            (if (> time-penalty u100) u0 (- u100 time-penalty))
          )
        )
        u50 ;; Not yet completed
      ))
    )
      (some {
        overall-effectiveness: (/ (+ implementation-score cost-efficiency community-score timeliness-score) u4),
        implementation-score: implementation-score,
        cost-efficiency: cost-efficiency,
        community-satisfaction: community-score,
        timeliness-score: timeliness-score
      })
    )
    none
  )
)

;; Get impact summary for multiple proposals
(define-read-only (get-impact-summary (proposal-ids (list 10 uint)))
  (map get-proposal-impact proposal-ids)
)

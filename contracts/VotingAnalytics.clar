;; Voting Analytics - Comprehensive analytics for referendum participation
;; Tracks voting patterns, participation rates, and governance insights

;; Error constants
(define-constant err-not-authorized (err u400))
(define-constant err-invalid-period (err u401))
(define-constant err-no-data (err u402))
(define-constant err-already-recorded (err u403))

;; Data variables
(define-data-var admin principal tx-sender)
(define-data-var analytics-enabled bool true)

;; Time period constants for analytics
(define-constant period-daily u144)    ;; ~1 day in blocks
(define-constant period-weekly u1008)  ;; ~1 week in blocks  
(define-constant period-monthly u4320) ;; ~1 month in blocks

;; Voting participation tracking
(define-map participation-stats
    uint ;; time-period (block-height / period)
    {
        total-eligible-voters: uint,
        unique-voters: uint,
        total-votes-cast: uint,
        proposals-created: uint,
        proposals-passed: uint,
        average-vote-weight: uint,
        participation-rate: uint
    }
)

;; Individual voter behavior patterns
(define-map voter-patterns
    principal
    {
        total-votes: uint,
        consecutive-votes: uint,
        favorite-voting-time: uint,
        average-vote-delay: uint,
        participation-streak: uint,
        last-vote-period: uint,
        voting-consistency: uint
    }
)

;; Proposal analytics
(define-map proposal-analytics
    uint ;; proposal-id
    {
        voting-velocity: uint,
        peak-voting-period: uint,
        final-participation: uint,
        controversy-score: uint,
        debate-intensity: uint,
        outcome-confidence: uint
    }
)

;; Demographic voting patterns
(define-map voting-demographics
    {period: uint, voter-type: (string-ascii 20)}
    {
        vote-count: uint,
        yes-preference: uint,
        average-engagement: uint,
        influence-score: uint
    }
)

;; Governance health metrics
(define-map governance-health
    uint ;; period
    {
        decentralization-score: uint,
        voter-diversity: uint,
        proposal-quality: uint,
        system-efficiency: uint,
        overall-health: uint
    }
)

;; Read-only functions
(define-read-only (get-participation-stats (period uint))
    (map-get? participation-stats period))

(define-read-only (get-voter-patterns (voter principal))
    (map-get? voter-patterns voter))

(define-read-only (get-proposal-analytics (proposal-id uint))
    (map-get? proposal-analytics proposal-id))

(define-read-only (get-governance-health (period uint))
    (map-get? governance-health period))

(define-read-only (calculate-current-period)
    (/ stacks-block-height period-weekly))

;; Record vote for analytics
(define-public (record-vote-analytics (proposal-id uint) (voter principal) (vote-weight uint))
    (let (
        (current-period (calculate-current-period))
        (current-stats (default-to 
            {total-eligible-voters: u0, unique-voters: u0, total-votes-cast: u0,
             proposals-created: u0, proposals-passed: u0, average-vote-weight: u0, participation-rate: u0}
            (map-get? participation-stats current-period)))
        (voter-data (default-to 
            {total-votes: u0, consecutive-votes: u0, favorite-voting-time: u0,
             average-vote-delay: u0, participation-streak: u0, last-vote-period: u0, voting-consistency: u0}
            (map-get? voter-patterns voter)))
    )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        
        ;; Update participation stats
        (map-set participation-stats current-period
            (merge current-stats {
                unique-voters: (+ (get unique-voters current-stats) u1),
                total-votes-cast: (+ (get total-votes-cast current-stats) u1),
                average-vote-weight: (/ (+ (* (get average-vote-weight current-stats) (get total-votes-cast current-stats)) vote-weight)
                                        (+ (get total-votes-cast current-stats) u1))
            })
        )
        
        ;; Update voter patterns
        (let (
            (consecutive-bonus (if (is-eq (get last-vote-period voter-data) (- current-period u1))
                (+ (get consecutive-votes voter-data) u1) u1))
            (new-streak (if (> consecutive-bonus u5) (+ (get participation-streak voter-data) u1)
                         (get participation-streak voter-data)))
        )
            (map-set voter-patterns voter
                (merge voter-data {
                    total-votes: (+ (get total-votes voter-data) u1),
                    consecutive-votes: consecutive-bonus,
                    last-vote-period: current-period,
                    participation-streak: new-streak,
                    voting-consistency: (/ (* (get total-votes voter-data) u100) current-period)
                })
            )
        )
        (ok true)
    )
)

;; Record proposal creation for analytics
(define-public (record-proposal-creation (proposal-id uint) (creator principal))
    (let (
        (current-period (calculate-current-period))
        (current-stats (default-to 
            {total-eligible-voters: u0, unique-voters: u0, total-votes-cast: u0,
             proposals-created: u0, proposals-passed: u0, average-vote-weight: u0, participation-rate: u0}
            (map-get? participation-stats current-period)))
    )
        (asserts! (var-get analytics-enabled) err-not-authorized)
        
        ;; Update proposal creation stats
        (map-set participation-stats current-period
            (merge current-stats {
                proposals-created: (+ (get proposals-created current-stats) u1)
            })
        )
        
        ;; Initialize proposal analytics
        (map-set proposal-analytics proposal-id
            {
                voting-velocity: u0,
                peak-voting-period: u0,
                final-participation: u0,
                controversy-score: u0,
                debate-intensity: u0,
                outcome-confidence: u0
            }
        )
        (ok true)
    )
)

;; Calculate governance health metrics
(define-public (calculate-governance-health (period uint))
    (let (
        (stats (unwrap! (get-participation-stats period) err-no-data))
        (participation (get participation-rate stats))
        (proposal-success-rate (if (> (get proposals-created stats) u0)
            (/ (* (get proposals-passed stats) u100) (get proposals-created stats)) u0))
    )
        (asserts! (is-eq tx-sender (var-get admin)) err-not-authorized)
        
        (let (
            (decentralization-raw (+ participation u20))
            (decentralization (if (> decentralization-raw u100) u100 decentralization-raw))
            (diversity-raw (/ (get unique-voters stats) u2))
            (diversity (if (> diversity-raw u100) u100 diversity-raw))
            (quality proposal-success-rate)
            (efficiency-raw (if (> (get average-vote-weight stats) u0) 
                (* (get average-vote-weight stats) u2) u50))
            (efficiency (if (> efficiency-raw u100) u100 efficiency-raw))
            (overall (/ (+ decentralization diversity quality efficiency) u4))
        )
            (map-set governance-health period
                {
                    decentralization-score: decentralization,
                    voter-diversity: diversity,
                    proposal-quality: quality,
                    system-efficiency: efficiency,
                    overall-health: overall
                }
            )
            (ok overall)
        )
    )
)

;; Get voting insights for a voter
(define-read-only (get-voter-insights (voter principal))
    (match (get-voter-patterns voter)
        patterns (some {
            engagement-level: (if (> (get total-votes patterns) u10) "high"
                             (if (> (get total-votes patterns) u3) "medium" "low")),
            consistency-rating: (get voting-consistency patterns),
            streak-status: (get participation-streak patterns),
            voting-behavior: (if (> (get consecutive-votes patterns) u5) "regular" "sporadic")
        })
        none
    )
)

;; Admin functions
(define-public (toggle-analytics (enabled bool))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) err-not-authorized)
        (var-set analytics-enabled enabled)
        (ok enabled)
    )
)

(define-read-only (get-analytics-status)
    (var-get analytics-enabled))

;; Calculate participation rate for current period
(define-read-only (get-current-participation-rate)
    (let (
        (current-period (calculate-current-period))
        (stats (get-participation-stats current-period))
    )
        (match stats
            data (let ((eligible (get total-eligible-voters data)))
                (/ (* (get unique-voters data) u100) (if (> eligible u1) eligible u1)))
            u0
        )
    )
)

;; title: referendum

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-INVALID-PROPOSAL (err u102))
(define-constant ERR-PROPOSAL-ENDED (err u103))
(define-constant ERR-MIN-VOTES-NOT-MET (err u104))

(define-data-var admin principal tx-sender)
(define-data-var proposal-counter uint u0)
(define-data-var min-votes uint u100)


(define-constant ERR-REPUTATION-OVERFLOW (err u107))
(define-constant ERR-MILESTONE-NOT-FOUND (err u108))
(define-constant ERR-MILESTONE-ALREADY-CLAIMED (err u109))
(define-constant ERR-INSUFFICIENT-REPUTATION (err u110))

(define-data-var milestone-counter uint u0)

(define-map stakeholder-reputation
    principal
    {
        total-score: uint,
        proposals-created: uint,
        votes-cast: uint,
        proposals-passed: uint,
        consecutive-votes: uint,
        last-vote-block: uint,
        badges: (list 20 (string-ascii 30))
    }
)

(define-map milestone-definitions
    uint
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        requirement-type: (string-ascii 20),
        threshold: uint,
        reward-points: uint,
        badge-title: (string-ascii 30),
        creator: principal,
        active: bool
    }
)

(define-map milestone-achievements
    { milestone-id: uint, user: principal }
    {
        achieved-at: uint,
        claimed: bool
    }
)

(define-map reputation-multipliers
    principal
    {
        proposal-bonus: uint,
        vote-bonus: uint,
        consecutive-bonus: uint,
        active-until: uint
    }
)

(define-map proposals
    uint 
    {
        title: (string-ascii 100),
        description: (string-ascii 500),
        creator: principal,
        start-block: uint,
        end-block: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 20)
    }
)

(define-map votes 
    { proposal-id: uint, voter: principal } 
    { choice: bool }
)

(define-read-only (get-proposal (proposal-id uint))
    (map-get? proposals proposal-id)
)

(define-read-only (get-vote (proposal-id uint) (voter principal))
    (map-get? votes { proposal-id: proposal-id, voter: voter })
)

(define-read-only (get-proposal-count)
    (var-get proposal-counter)
)

(define-read-only (get-min-votes)
    (var-get min-votes)
)

(define-read-only (is-admin)
    (is-eq tx-sender (var-get admin))
)

(define-public (create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (blocks uint))
    (let ((new-id (+ (var-get proposal-counter) u1)))
        (map-set proposals new-id
            {
                title: title,
                description: description,
                creator: tx-sender,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height blocks),
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            }
        )
        (var-set proposal-counter new-id)
        (ok new-id)
    )
)

(define-public (vote (proposal-id uint) (choice bool))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (vote-key { proposal-id: proposal-id, voter: tx-sender })
    )
        (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        
        (map-set votes vote-key { choice: choice })
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    yes-votes: (if choice (+ (get yes-votes proposal) u1) (get yes-votes proposal)),
                    no-votes: (if (not choice) (+ (get no-votes proposal) u1) (get no-votes proposal))
                }
            )
        )
        (ok true)
    )
)

(define-public (finalize-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
    )
        (asserts! (>= stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        (asserts! (>= total-votes (var-get min-votes)) ERR-MIN-VOTES-NOT-MET)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    status: (if (> (get yes-votes proposal) (get no-votes proposal))
                        "passed"
                        "rejected"
                    )
                }
            )
        )
        (ok true)
    )
)

(define-public (set-admin (new-admin principal))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set admin new-admin)
        (ok true)
    )
)

(define-public (set-min-votes (new-min uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (var-set min-votes new-min)
        (ok true)
    )
)

(define-map user-registration
    principal 
    { registration-height: uint }
)

(define-map delegations
    principal
    { delegate: principal }
)

(define-read-only (get-delegate (voter principal))
    (get delegate (map-get? delegations voter))
)

(define-read-only (get-vote-weight (voter principal))
    (let (
        (registration (default-to { registration-height: stacks-block-height } 
            (map-get? user-registration voter)))
        (blocks-registered (- stacks-block-height (get registration-height registration)))
    )
        (+ u1 (/ blocks-registered u1000))
    )
)

(define-public (register-for-voting)
    (begin
        (map-set user-registration tx-sender { registration-height: stacks-block-height })
        (ok true)
    )
)

;; Modified vote function to include weight
(define-public (vote-new (proposal-id uint) (choice bool))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (delegate (get-delegate tx-sender))
        (vote-key { proposal-id: proposal-id, voter: tx-sender })
        (weight (get-vote-weight tx-sender))
    )
        (asserts! (or (is-none delegate) (is-eq tx-sender (unwrap! delegate ERR-NOT-AUTHORIZED))) (err u201))
        (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        
        (map-set votes vote-key { choice: choice })
        (map-set proposals proposal-id
            (merge proposal 
                {
                    yes-votes: (if choice (+ (get yes-votes proposal) weight) (get yes-votes proposal)),
                    no-votes: (if (not choice) (+ (get no-votes proposal) weight) (get no-votes proposal))
                }
            )
        )
        (ok true)
    )
)
;; 

(define-map proposal-categories
    uint
    (string-ascii 20)
)

(define-map category-proposals
    (string-ascii 20)
    (list 100 uint)
)

(define-public (create-proposal-new (title (string-ascii 100)) (description (string-ascii 500)) (blocks uint) (category (string-ascii 20)))
    (let (
        (new-id (+ (var-get proposal-counter) u1))
        (current-list (default-to (list) (map-get? category-proposals category)))
    )
        (map-set proposals new-id
            {
                title: title,
                description: description,
                creator: tx-sender,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height blocks),
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            }
        )
        (map-set proposal-categories new-id category)
        ;; (map-set category-proposals category (append current-list new-id))
        (var-set proposal-counter new-id)
        (ok new-id)
    )
)

(define-read-only (get-proposals-by-category (category (string-ascii 20)))
    (default-to (list) (map-get? category-proposals category))
)


(define-constant ERR-TEMPLATE-NOT-FOUND (err u105))
(define-constant ERR-INVALID-TEMPLATE-DATA (err u106))

(define-data-var template-counter uint u0)

(define-map proposal-templates
    uint
    {
        name: (string-ascii 50),
        description: (string-ascii 200),
        required-fields: (list 10 (string-ascii 30)),
        min-duration: uint,
        max-duration: uint,
        min-votes-override: (optional uint),
        creator: principal,
        active: bool
    }
)

(define-map template-proposals
    uint
    {
        template-id: uint,
        template-data: (string-ascii 1000)
    }
)

(define-read-only (get-template (template-id uint))
    (map-get? proposal-templates template-id)
)

(define-read-only (get-template-count)
    (var-get template-counter)
)

(define-read-only (get-proposal-template-data (proposal-id uint))
    (map-get? template-proposals proposal-id)
)

(define-public (create-template 
    (name (string-ascii 50))
    (description (string-ascii 200))
    (required-fields (list 10 (string-ascii 30)))
    (min-duration uint)
    (max-duration uint)
    (min-votes-override (optional uint)))
    (let ((new-template-id (+ (var-get template-counter) u1)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (asserts! (<= min-duration max-duration) ERR-INVALID-TEMPLATE-DATA)
        
        (map-set proposal-templates new-template-id
            {
                name: name,
                description: description,
                required-fields: required-fields,
                min-duration: min-duration,
                max-duration: max-duration,
                min-votes-override: min-votes-override,
                creator: tx-sender,
                active: true
            }
        )
        (var-set template-counter new-template-id)
        (ok new-template-id)
    )
)

(define-public (create-proposal-from-template 
    (template-id uint)
    (title (string-ascii 100))
    (description (string-ascii 500))
    (blocks uint)
    (template-data (string-ascii 1000)))
    (let (
        (template (unwrap! (get-template template-id) ERR-TEMPLATE-NOT-FOUND))
        (new-id (+ (var-get proposal-counter) u1))
    )
        (asserts! (get active template) ERR-TEMPLATE-NOT-FOUND)
        (asserts! (>= blocks (get min-duration template)) ERR-INVALID-TEMPLATE-DATA)
        (asserts! (<= blocks (get max-duration template)) ERR-INVALID-TEMPLATE-DATA)
        
        (map-set proposals new-id
            {
                title: title,
                description: description,
                creator: tx-sender,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height blocks),
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            }
        )
        
        (map-set template-proposals new-id
            {
                template-id: template-id,
                template-data: template-data
            }
        )
        
        (var-set proposal-counter new-id)
        (ok new-id)
    )
)

(define-public (toggle-template-status (template-id uint))
    (let ((template (unwrap! (get-template template-id) ERR-TEMPLATE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        
        (map-set proposal-templates template-id
            (merge template { active: (not (get active template)) })
        )
        (ok true)
    )
)

(define-public (finalize-template-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (template-info (map-get? template-proposals proposal-id))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        (required-votes 
            (if (is-some template-info)
                (let ((template-data (unwrap-panic template-info)))
                    (let ((template (unwrap-panic (get-template (get template-id template-data)))))
                        (default-to (var-get min-votes) (get min-votes-override template))
                    )
                )
                (var-get min-votes)
            )
        )
    )
        (asserts! (>= stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        (asserts! (>= total-votes required-votes) ERR-MIN-VOTES-NOT-MET)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    status: (if (> (get yes-votes proposal) (get no-votes proposal))
                        "passed"
                        "rejected"
                    )
                }
            )
        )
        (ok true)
    )
)


(define-read-only (get-stakeholder-reputation (user principal))
    (default-to
        {
            total-score: u0,
            proposals-created: u0,
            votes-cast: u0,
            proposals-passed: u0,
            consecutive-votes: u0,
            last-vote-block: u0,
            badges: (list)
        }
        (map-get? stakeholder-reputation user)
    )
)

(define-read-only (get-milestone-definition (milestone-id uint))
    (map-get? milestone-definitions milestone-id)
)

(define-read-only (get-milestone-achievement (milestone-id uint) (user principal))
    (map-get? milestone-achievements { milestone-id: milestone-id, user: user })
)

(define-read-only (get-reputation-multiplier (user principal))
    (default-to
        {
            proposal-bonus: u100,
            vote-bonus: u100,
            consecutive-bonus: u100,
            active-until: u0
        }
        (map-get? reputation-multipliers user)
    )
)

(define-read-only (calculate-reputation-score (user principal))
    (let (
        (reputation (get-stakeholder-reputation user))
        (multiplier (get-reputation-multiplier user))
    )
        (+ 
            (* (get proposals-created reputation) (get proposal-bonus multiplier))
            (* (get votes-cast reputation) (get vote-bonus multiplier))
            (* (get proposals-passed reputation) u500)
            (* (get consecutive-votes reputation) (get consecutive-bonus multiplier))
        )
    )
)

(define-public (create-milestone
    (name (string-ascii 50))
    (description (string-ascii 200))
    (requirement-type (string-ascii 20))
    (threshold uint)
    (reward-points uint)
    (badge-title (string-ascii 30)))
    (let ((new-milestone-id (+ (var-get milestone-counter) u1)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        
        (map-set milestone-definitions new-milestone-id
            {
                name: name,
                description: description,
                requirement-type: requirement-type,
                threshold: threshold,
                reward-points: reward-points,
                badge-title: badge-title,
                creator: tx-sender,
                active: true
            }
        )
        (var-set milestone-counter new-milestone-id)
        (ok new-milestone-id)
    )
)

(define-public (claim-milestone (milestone-id uint))
    (let (
        (milestone (unwrap! (get-milestone-definition milestone-id) ERR-MILESTONE-NOT-FOUND))
        (user-reputation (get-stakeholder-reputation tx-sender))
        (achievement-key { milestone-id: milestone-id, user: tx-sender })
    )
        (asserts! (get active milestone) ERR-MILESTONE-NOT-FOUND)
        (asserts! (is-none (get-milestone-achievement milestone-id tx-sender)) ERR-MILESTONE-ALREADY-CLAIMED)
        
        (let (
            (requirement-met
                (if (is-eq (get requirement-type milestone) "proposals")
                    (>= (get proposals-created user-reputation) (get threshold milestone))
                    (if (is-eq (get requirement-type milestone) "votes")
                        (>= (get votes-cast user-reputation) (get threshold milestone))
                        (if (is-eq (get requirement-type milestone) "passed")
                            (>= (get proposals-passed user-reputation) (get threshold milestone))
                            (>= (get consecutive-votes user-reputation) (get threshold milestone))
                        )
                    )
                )
            )
        )
            (asserts! requirement-met ERR-INSUFFICIENT-REPUTATION)
            
            (map-set milestone-achievements achievement-key
                {
                    achieved-at: stacks-block-height,
                    claimed: true
                }
            )
            
            (let (
                (current-badges (get badges user-reputation))
                (new-badges (unwrap! (as-max-len? (append current-badges (get badge-title milestone)) u20) ERR-REPUTATION-OVERFLOW))
            )
                (map-set stakeholder-reputation tx-sender
                    (merge user-reputation
                        {
                            total-score: (+ (get total-score user-reputation) (get reward-points milestone)),
                            badges: new-badges
                        }
                    )
                )
            )
            (ok true)
        )
    )
)

(define-public (update-reputation-on-proposal (proposal-id uint))
    (let (
        (user-reputation (get-stakeholder-reputation tx-sender))
        (multiplier (get-reputation-multiplier tx-sender))
        (base-points u100)
        (bonus-points (/ (* base-points (get proposal-bonus multiplier)) u100))
    )
        (map-set stakeholder-reputation tx-sender
            (merge user-reputation
                {
                    total-score: (+ (get total-score user-reputation) bonus-points),
                    proposals-created: (+ (get proposals-created user-reputation) u1)
                }
            )
        )
        (ok true)
    )
)

(define-public (update-reputation-on-vote (proposal-id uint))
    (let (
        (user-reputation (get-stakeholder-reputation tx-sender))
        (multiplier (get-reputation-multiplier tx-sender))
        (base-points u50)
        (bonus-points (/ (* base-points (get vote-bonus multiplier)) u100))
        (consecutive-bonus 
            (if (is-eq (+ (get last-vote-block user-reputation) u1) stacks-block-height)
                (+ (get consecutive-votes user-reputation) u1)
                u0
            )
        )
    )
        (map-set stakeholder-reputation tx-sender
            (merge user-reputation
                {
                    total-score: (+ (get total-score user-reputation) bonus-points),
                    votes-cast: (+ (get votes-cast user-reputation) u1),
                    consecutive-votes: consecutive-bonus,
                    last-vote-block: stacks-block-height
                }
            )
        )
        (ok true)
    )
)

(define-public (update-reputation-on-proposal-passed (proposal-id uint) (creator principal))
    (let (
        (user-reputation (get-stakeholder-reputation creator))
        (base-points u200)
    )
        (map-set stakeholder-reputation creator
            (merge user-reputation
                {
                    total-score: (+ (get total-score user-reputation) base-points),
                    proposals-passed: (+ (get proposals-passed user-reputation) u1)
                }
            )
        )
        (ok true)
    )
)

(define-public (set-reputation-multiplier 
    (user principal)
    (proposal-bonus uint)
    (vote-bonus uint)
    (consecutive-bonus uint)
    (duration uint))
    (begin
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        (map-set reputation-multipliers user
            {
                proposal-bonus: proposal-bonus,
                vote-bonus: vote-bonus,
                consecutive-bonus: consecutive-bonus,
                active-until: (+ stacks-block-height duration)
            }
        )
        (ok true)
    )
)

(define-public (toggle-milestone-status (milestone-id uint))
    (let ((milestone (unwrap! (get-milestone-definition milestone-id) ERR-MILESTONE-NOT-FOUND)))
        (asserts! (is-eq tx-sender (var-get admin)) ERR-NOT-AUTHORIZED)
        
        (map-set milestone-definitions milestone-id
            (merge milestone { active: (not (get active milestone)) })
        )
        (ok true)
    )
)

(define-public (reputation-enhanced-vote (proposal-id uint) (choice bool))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (delegate (get-delegate tx-sender))
        (vote-key { proposal-id: proposal-id, voter: tx-sender })
        (weight (get-vote-weight tx-sender))
        (reputation-score (calculate-reputation-score tx-sender))
        (reputation-weight (+ weight (/ reputation-score u1000)))
    )
        (asserts! (or (is-none delegate) (is-eq tx-sender (unwrap! delegate ERR-NOT-AUTHORIZED))) (err u201))
        (asserts! (is-none (get-vote proposal-id tx-sender)) ERR-ALREADY-VOTED)
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        
        (map-set votes vote-key { choice: choice })
        (map-set proposals proposal-id
            (merge proposal 
                {
                    yes-votes: (if choice (+ (get yes-votes proposal) reputation-weight) (get yes-votes proposal)),
                    no-votes: (if (not choice) (+ (get no-votes proposal) reputation-weight) (get no-votes proposal))
                }
            )
        )
        (unwrap! (update-reputation-on-vote proposal-id) ERR-REPUTATION-OVERFLOW)
        (ok true)
    )
)

(define-public (reputation-enhanced-create-proposal (title (string-ascii 100)) (description (string-ascii 500)) (blocks uint))
    (let ((new-id (+ (var-get proposal-counter) u1)))
        (map-set proposals new-id
            {
                title: title,
                description: description,
                creator: tx-sender,
                start-block: stacks-block-height,
                end-block: (+ stacks-block-height blocks),
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            }
        )
        (var-set proposal-counter new-id)
        (unwrap! (update-reputation-on-proposal new-id) ERR-REPUTATION-OVERFLOW)
        (ok new-id)
    )
)

(define-public (reputation-enhanced-finalize-proposal (proposal-id uint))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (total-votes (+ (get yes-votes proposal) (get no-votes proposal)))
        (proposal-passed (> (get yes-votes proposal) (get no-votes proposal)))
    )
        (asserts! (>= stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        (asserts! (>= total-votes (var-get min-votes)) ERR-MIN-VOTES-NOT-MET)
        
        (map-set proposals proposal-id
            (merge proposal 
                {
                    status: (if proposal-passed "passed" "rejected")
                }
            )
        )
        
        (if proposal-passed
            (unwrap! (update-reputation-on-proposal-passed proposal-id (get creator proposal)) ERR-REPUTATION-OVERFLOW)
            true
        )
        (ok true)
    )
)

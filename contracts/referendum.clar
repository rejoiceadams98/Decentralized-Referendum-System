
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
;; Amendment System for Proposal Refinement

;; Amendment-specific error codes
(define-constant ERR-AMENDMENT-NOT-FOUND (err u111))
(define-constant ERR-CANNOT-AMEND-ENDED-PROPOSAL (err u112))
(define-constant ERR-AMENDMENT-ALREADY-VOTED (err u113))
(define-constant ERR-AMENDMENT-EXPIRED (err u114))
(define-constant ERR-CANNOT-AMEND-OWN-PROPOSAL (err u115))

;; Amendment counter for unique IDs
(define-data-var amendment-counter uint u0)

;; Maps to store amendment data
(define-map proposal-amendments
    uint ;; proposal-id
    {
        amendment-ids: (list 50 uint),
        active-amendment: (optional uint),
        amendment-count: uint
    }
)

(define-map amendments
    uint ;; amendment-id
    {
        proposal-id: uint,
        proposer: principal,
        new-title: (optional (string-ascii 100)),
        new-description: (optional (string-ascii 500)),
        duration-extension: (optional uint),
        created-at: uint,
        expires-at: uint,
        yes-votes: uint,
        no-votes: uint,
        status: (string-ascii 20) ;; "active", "approved", "rejected", "expired"
    }
)

(define-map amendment-votes
    { amendment-id: uint, voter: principal }
    { choice: bool, voted-at: uint }
)

(define-map amendment-approvals
    { proposal-id: uint, original-creator: principal }
    { approved-amendments: (list 10 uint) }
)

;; Read-only functions for amendment data retrieval
(define-read-only (get-amendment (amendment-id uint))
    (map-get? amendments amendment-id)
)

(define-read-only (get-proposal-amendments (proposal-id uint))
    (default-to
        {
            amendment-ids: (list),
            active-amendment: none,
            amendment-count: u0
        }
        (map-get? proposal-amendments proposal-id)
    )
)

(define-read-only (get-amendment-vote (amendment-id uint) (voter principal))
    (map-get? amendment-votes { amendment-id: amendment-id, voter: voter })
)

(define-read-only (get-amendment-count)
    (var-get amendment-counter)
)

(define-read-only (get-creator-approvals (proposal-id uint) (creator principal))
    (default-to
        { approved-amendments: (list) }
        (map-get? amendment-approvals { proposal-id: proposal-id, original-creator: creator })
    )
)

;; Check if amendment is still active and votable
(define-read-only (is-amendment-active (amendment-id uint))
    (match (get-amendment amendment-id)
        amendment (and 
            (is-eq (get status amendment) "active")
            (< stacks-block-height (get expires-at amendment))
        )
        false
    )
)

;; Create a new amendment to an existing proposal
(define-public (create-amendment 
    (proposal-id uint)
    (new-title (optional (string-ascii 100)))
    (new-description (optional (string-ascii 500)))
    (duration-extension (optional uint)))
    (let (
        (proposal (unwrap! (get-proposal proposal-id) ERR-INVALID-PROPOSAL))
        (new-amendment-id (+ (var-get amendment-counter) u1))
        (current-amendments (get-proposal-amendments proposal-id))
        (amendment-duration u1440) ;; ~1 day in blocks
    )
        ;; Validate proposal is still active
        (asserts! (< stacks-block-height (get end-block proposal)) ERR-PROPOSAL-ENDED)
        (asserts! (is-eq (get status proposal) "active") ERR-CANNOT-AMEND-ENDED-PROPOSAL)
        
        ;; Prevent self-amendment (encourages collaboration)
        (asserts! (not (is-eq tx-sender (get creator proposal))) ERR-CANNOT-AMEND-OWN-PROPOSAL)
        
        ;; Ensure at least one modification is proposed
        (asserts! (or (is-some new-title) (is-some new-description) (is-some duration-extension)) ERR-INVALID-PROPOSAL)
        
        ;; Create the amendment
        (map-set amendments new-amendment-id
            {
                proposal-id: proposal-id,
                proposer: tx-sender,
                new-title: new-title,
                new-description: new-description,
                duration-extension: duration-extension,
                created-at: stacks-block-height,
                expires-at: (+ stacks-block-height amendment-duration),
                yes-votes: u0,
                no-votes: u0,
                status: "active"
            }
        )
        
        ;; Update proposal amendment tracking
        (let (
            (updated-ids (unwrap! (as-max-len? 
                (append (get amendment-ids current-amendments) new-amendment-id) u50) 
                ERR-INVALID-PROPOSAL))
        )
            (map-set proposal-amendments proposal-id
                (merge current-amendments
                    {
                        amendment-ids: updated-ids,
                        amendment-count: (+ (get amendment-count current-amendments) u1)
                    }
                )
            )
        )
        
        (var-set amendment-counter new-amendment-id)
        (ok new-amendment-id)
    )
)

;; Vote on an amendment
(define-public (vote-on-amendment (amendment-id uint) (choice bool))
    (let (
        (amendment (unwrap! (get-amendment amendment-id) ERR-AMENDMENT-NOT-FOUND))
        (vote-key { amendment-id: amendment-id, voter: tx-sender })
        (voter-weight (get-vote-weight tx-sender))
    )
        ;; Validate amendment is active and votable
        (asserts! (is-amendment-active amendment-id) ERR-AMENDMENT-EXPIRED)
        (asserts! (is-none (get-amendment-vote amendment-id tx-sender)) ERR-AMENDMENT-ALREADY-VOTED)
        
        ;; Record the vote
        (map-set amendment-votes vote-key 
            { choice: choice, voted-at: stacks-block-height }
        )
        
        ;; Update vote counts with weight
        (map-set amendments amendment-id
            (merge amendment
                {
                    yes-votes: (if choice 
                        (+ (get yes-votes amendment) voter-weight) 
                        (get yes-votes amendment)),
                    no-votes: (if (not choice) 
                        (+ (get no-votes amendment) voter-weight) 
                        (get no-votes amendment))
                }
            )
        )
        (ok true)
    )
)

;; Original proposal creator can directly approve an amendment
(define-public (approve-amendment (amendment-id uint))
    (let (
        (amendment (unwrap! (get-amendment amendment-id) ERR-AMENDMENT-NOT-FOUND))
        (proposal (unwrap! (get-proposal (get proposal-id amendment)) ERR-INVALID-PROPOSAL))
        (approval-key { proposal-id: (get proposal-id amendment), original-creator: tx-sender })
        (current-approvals (get-creator-approvals (get proposal-id amendment) tx-sender))
    )
        ;; Only original proposal creator can approve
        (asserts! (is-eq tx-sender (get creator proposal)) ERR-NOT-AUTHORIZED)
        (asserts! (is-amendment-active amendment-id) ERR-AMENDMENT-EXPIRED)
        
        ;; Mark amendment as approved
        (map-set amendments amendment-id
            (merge amendment { status: "approved" })
        )
        
        ;; Track creator approval
        (let (
            (updated-approvals (unwrap! (as-max-len? 
                (append (get approved-amendments current-approvals) amendment-id) u10)
                ERR-INVALID-PROPOSAL))
        )
            (map-set amendment-approvals approval-key
                { approved-amendments: updated-approvals }
            )
        )
        (ok true)
    )
)

;; Finalize amendment voting and apply if successful
(define-public (finalize-amendment (amendment-id uint))
    (let (
        (amendment (unwrap! (get-amendment amendment-id) ERR-AMENDMENT-NOT-FOUND))
        (proposal (unwrap! (get-proposal (get proposal-id amendment)) ERR-INVALID-PROPOSAL))
        (total-votes (+ (get yes-votes amendment) (get no-votes amendment)))
        (approval-threshold (/ (var-get min-votes) u2)) ;; 50% of min votes required
        (amendment-passed (and 
            (>= total-votes approval-threshold)
            (> (get yes-votes amendment) (get no-votes amendment))
        ))
    )
        ;; Can only finalize after amendment period expires or if creator approved
        (asserts! (or 
            (>= stacks-block-height (get expires-at amendment))
            (is-eq (get status amendment) "approved")
        ) ERR-AMENDMENT-EXPIRED)
        
        (if (or amendment-passed (is-eq (get status amendment) "approved"))
            (begin
                ;; Apply amendment to original proposal
                (unwrap! (apply-amendment-to-proposal amendment-id) ERR-INVALID-PROPOSAL)
                
                ;; Mark amendment as approved
                (map-set amendments amendment-id
                    (merge amendment { status: "approved" })
                )
                (ok true)
            )
            (begin
                ;; Mark amendment as rejected
                (map-set amendments amendment-id
                    (merge amendment { status: "rejected" })
                )
                (ok false)
            )
        )
    )
)

;; Internal function to apply approved amendment to proposal
(define-private (apply-amendment-to-proposal (amendment-id uint))
    (let (
        (amendment (unwrap! (get-amendment amendment-id) ERR-AMENDMENT-NOT-FOUND))
        (proposal (unwrap! (get-proposal (get proposal-id amendment)) ERR-INVALID-PROPOSAL))
    )
        (map-set proposals (get proposal-id amendment)
            (merge proposal
                {
                    title: (default-to (get title proposal) (get new-title amendment)),
                    description: (default-to (get description proposal) (get new-description amendment)),
                    end-block: (match (get duration-extension amendment)
                        extension (+ (get end-block proposal) extension)
                        (get end-block proposal)
                    )
                }
            )
        )
        (ok true)
    )
)

;; Batch reject expired amendments for cleanup
(define-public (cleanup-expired-amendments (amendment-ids (list 10 uint)))
    (begin
        (map cleanup-single-amendment amendment-ids)
        (ok true)
    )
)

;; Helper function for cleanup
(define-private (cleanup-single-amendment (amendment-id uint))
    (match (get-amendment amendment-id)
        amendment (if (and 
            (is-eq (get status amendment) "active")
            (>= stacks-block-height (get expires-at amendment))
        )
            (map-set amendments amendment-id
                (merge amendment { status: "expired" })
            )
            true
        )
        true
    )
)

;; Get amendment voting results summary
(define-read-only (get-amendment-results (amendment-id uint))
    (match (get-amendment amendment-id)
        amendment (let (
            (total-votes (+ (get yes-votes amendment) (get no-votes amendment)))
        )
            (some {
                amendment-id: amendment-id,
                proposal-id: (get proposal-id amendment),
                total-votes: total-votes,
                yes-percentage: (if (> total-votes u0) 
                    (/ (* (get yes-votes amendment) u100) total-votes)
                    u0
                ),
                status: (get status amendment),
                expires-at: (get expires-at amendment)
            })
        )
        none
    )
)



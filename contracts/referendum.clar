
;; title: referendum

(define-constant ERR-NOT-AUTHORIZED (err u100))
(define-constant ERR-ALREADY-VOTED (err u101))
(define-constant ERR-INVALID-PROPOSAL (err u102))
(define-constant ERR-PROPOSAL-ENDED (err u103))
(define-constant ERR-MIN-VOTES-NOT-MET (err u104))

(define-data-var admin principal tx-sender)
(define-data-var proposal-counter uint u0)
(define-data-var min-votes uint u100)

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
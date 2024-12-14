(define-constant STX_LOTTERY_FEE u1000)
(define-constant STX_LOTTERY_TICKET_PRICE u50000) 
(define-constant STX_LOTTERY_FEE_WALLET 'STWHWSFZ3WJS4V0HRW4BSYNXZ73PQDP1TQ7HGKZV)
(define-constant DRAW_TICKET_THRESHOLD u4)
(define-constant MAX_TICKETS_PER_USER u2)

(define-constant MATCH_4_PRIZE_PERCENT u50) ;; 50% total prize
(define-constant MATCH_3_PRIZE_PERCENT u30) ;; 30% total prize
(define-constant MATCH_2_PRIZE_PERCENT u20) ;; 20% total prize
(define-constant CLAIM_EXPIRE_TIME u1) ;; 30 days = 2592000s

(define-constant contract-principal (as-contract tx-sender))
(define-data-var contract-owner principal tx-sender) 
;; define error codes
(define-constant err-round-ended (err u100))
(define-constant err-ticket-sold-out (err u101))
(define-constant err-no-prize-to-claim (err u102))
(define-constant err-already-claimed (err u103))
(define-constant err-round-not-found (err u104))
(define-constant err-ticket-not-found (err u105))
(define-constant err-winning-numbers-not-found (err u106))
(define-constant err-matches-not-found (err u107))
(define-constant err-pick-four-numbers (err u108))
(define-constant err-can-not-end-round (err u109))
(define-constant err-append-ticket-ids (err u110))
(define-constant err-draw-numbers (err u111))
(define-constant err-start-new-round (err u112))
(define-constant err-can-not-get-hash (err u113))
(define-constant err-not-owner (err u114))
(define-constant err-too-many-tickets (err u115))
(define-constant err-already-started (err u116))
(define-constant err-block-info-not-found (err u117))

;; define data variables
(define-data-var current-round-id uint u0)
(define-data-var drawn-number (optional (list 4 uint)) none)
(define-data-var ticket-counter uint u1)
(define-data-var number-pool (list 32 uint) (list 
    u1 u2 u3 u4 u5 u6 u7 u8 u9 u10 u11 u12 u13 u14 u15 u16 
    u17 u18 u19 u20 u21 u22 u23 u24 u25 u26 u27 u28 u29 u30 u31 u32))

;; Store ticket IDs for each round
(define-map round-ticket-ids uint (list 7 {user: principal, ticket-id: uint}))
(define-map lottery-rounds uint {
    total-tickets: uint,
    winning-numbers: (optional (list 4 uint)),
    is-ended: bool,
    total-prize: uint,
    no-winner-prize: uint,
    unclaimed-prize: uint,
    match-4-count: uint,
    match-3-count: uint,
    match-2-count: uint,
    start-block: uint,
    end-block: uint
})
(define-map user-tickets { round-id: uint, user: principal, ticket-id: uint } {
    numbers: (list 4 uint),
    matches: (optional uint),
    claimed: bool,
    prize-amount: (optional uint)
})
;; Track number of tickets per user per round
(define-map user-ticket-count {round-id: uint, user: principal} uint)

;; Get ticket IDs for a specific round
(define-read-only (get-round-ticket-ids (round-id uint))
    (map-get? round-ticket-ids round-id))

(define-read-only (get-drawn-number)
    (var-get drawn-number))

(define-read-only (get-lottery-round (round-id uint))
    (map-get? lottery-rounds round-id))

(define-read-only (get-current-round)
    (let ((round-id (var-get current-round-id)))
        (ok {
            round-id: round-id,
            round-data: (map-get? lottery-rounds round-id)
        })))

(define-read-only (get-user-ticket-count (round-id uint))
    (map-get? user-ticket-count {round-id: round-id, user: tx-sender}))

;; Get user ticket info
(define-read-only (get-user-ticket (round-id uint) (user principal) (ticket-id uint))
    (map-get? user-tickets {round-id: round-id, user: user, ticket-id: ticket-id}))

(define-public (start-lottery)
    (begin
        (let ((caller tx-sender)
              (current-round (var-get current-round-id)))
            (asserts! (is-eq caller (var-get contract-owner)) err-not-owner)
            (asserts! (is-eq current-round u0) err-already-started)
            
            (map-set lottery-rounds u1 {
                total-tickets: u0,
                winning-numbers: none,
                is-ended: false,
                total-prize: u0,
                no-winner-prize: u0,
                unclaimed-prize: u0,
                match-4-count: u0,
                match-3-count: u0,
                match-2-count: u0,
                start-block: stacks-block-height,
                end-block: u0,
            })

            (var-set current-round-id u1)

            (ok true))
    )
)

;; Count how many numbers in a ticket match the winning numbers
(define-private (count-matching-numbers (ticket-numbers (list 4 uint)) (winning-numbers (list 4 uint)))
    (let
        ((num1 (unwrap-panic (element-at ticket-numbers u0)))
         (num2 (unwrap-panic (element-at ticket-numbers u1)))
         (num3 (unwrap-panic (element-at ticket-numbers u2)))
         (num4 (unwrap-panic (element-at ticket-numbers u3))))
        (+ (if (is-some (index-of winning-numbers num1)) u1 u0)
           (if (is-some (index-of winning-numbers num2)) u1 u0)
           (if (is-some (index-of winning-numbers num3)) u1 u0)
           (if (is-some (index-of winning-numbers num4)) u1 u0))))

;; Public function to claim prize
(define-public (claim-prize (round-id uint) (ticket-id uint))
    (let
        ((user-ticket (unwrap! (map-get? user-tickets {round-id: round-id, user: tx-sender, ticket-id: ticket-id}) err-ticket-not-found))
         (lottery-round (unwrap! (map-get? lottery-rounds round-id) err-round-not-found))
         
         (winning-numbers (unwrap! (get winning-numbers lottery-round) err-winning-numbers-not-found))
         (matches (unwrap! (get matches user-ticket) err-matches-not-found))
         (recipent tx-sender)
         (prize-amount (unwrap! (get prize-amount user-ticket) err-no-prize-to-claim))
         (fee-amount (/ (* prize-amount u5) u100))
         (user-amount (- prize-amount fee-amount)))

        (asserts! (not (get claimed user-ticket)) err-already-claimed)
        
        (asserts! (get is-ended lottery-round) err-round-ended)

        (try! (as-contract (stx-transfer? user-amount contract-principal recipent)))

        (try! (as-contract (stx-transfer? fee-amount contract-principal STX_LOTTERY_FEE_WALLET)))

        (map-set user-tickets 
            {round-id: round-id, user: recipent, ticket-id: ticket-id}
            (merge user-ticket {claimed: true})
        )

        (map-set lottery-rounds round-id
            (merge lottery-round {
                unclaimed-prize: (- (get unclaimed-prize lottery-round) prize-amount)
            })
        )

        (ok true)
    )
)

(define-private (get-random-4-bytes (seed (buff 32)))
    (unwrap-panic (slice? seed u0 u4)))

(define-private (bytes-to-uint (bytes (buff 4)))
    (+ 
        (* (buff-to-uint-le (unwrap-panic (slice? bytes u0 u1))) u1)
        (* (buff-to-uint-le (unwrap-panic (slice? bytes u1 u2))) u256)
        (* (buff-to-uint-le (unwrap-panic (slice? bytes u2 u3))) u65536)
        (* (buff-to-uint-le (unwrap-panic (slice? bytes u3 u4))) u16777216)
    ))

(define-private (get-index-from-pool (bytes (buff 4)) (pool-size uint))
    (mod (buff-to-uint-be bytes) pool-size))

(define-private (remove-at-index (lst (list 32 uint)) (index uint))
    (let ((before-index (unwrap-panic (slice? lst u0 index)))
          (after-index (if (>= (+ index u1) (len lst))
                          (list)
                          (unwrap-panic (slice? lst (+ index u1) (len lst))))))
        (concat before-index after-index)))

(define-private (select-number (bytes (buff 4)) (pool (list 32 uint)))
    (let (
        (pool-size (len pool))
        (selected-index (get-index-from-pool bytes pool-size))
        (selected-number (unwrap-panic (element-at pool selected-index)))
        (new-pool (remove-at-index pool selected-index))
    )
    {number: selected-number,
            remaining-pool: new-pool,
            next-seed: (get-random-4-bytes (sha256 bytes))  ;; Generate next seed
        }
)
)

(define-private (pick-four-numbers (tx-seed (buff 4)))
    (let (
        (first-pick (select-number tx-seed (var-get number-pool)))
        
        (second-pick (select-number (unwrap-panic (as-max-len? (get next-seed first-pick) u4)) 
                                  (unwrap-panic (as-max-len? (get remaining-pool first-pick) u32))))
        
        (third-pick (select-number (unwrap-panic (as-max-len? (get next-seed second-pick) u4)) 
                                 (unwrap-panic (as-max-len? (get remaining-pool second-pick) u32))))
        
        (fourth-pick (select-number (unwrap-panic (as-max-len? (get next-seed third-pick) u4)) 
                                  (unwrap-panic (as-max-len? (get remaining-pool third-pick) u32))))
    )
    {first: (get number first-pick), second: (get number second-pick),
     third: (get number third-pick), fourth: (get number fourth-pick)}))


;; Calculate prize amount based on number of matches and total winners
(define-private (calculate-prize-amount (matches uint) (total-prize uint) (round-data {match-4-count: uint, match-3-count: uint, match-2-count: uint}))
    (let
        ((match-count (if (>= matches u2)
            (unwrap-panic (element-at 
                (list 
                    (get match-4-count round-data)
                    (get match-3-count round-data) 
                    (get match-2-count round-data))
                (- u4 matches)))
            u0))
         (prize-percent (if (>= matches u2)
            (unwrap-panic (element-at 
                (list MATCH_4_PRIZE_PERCENT
                      MATCH_3_PRIZE_PERCENT 
                      MATCH_2_PRIZE_PERCENT)
                (- u4 matches)))
            u0)))
        
        (if (> match-count u0)
            (/ (* total-prize prize-percent) (* u100 match-count))
            u0)
    )
)

;; Test function for calculate-prize-amount
(define-public (test-calculate-prize-amount (matches uint) (total-prize uint))
    (let
        ((test-round-data {
            match-4-count: u1,
            match-3-count: u2,
            match-2-count: u3
        }))
        (ok (calculate-prize-amount matches total-prize test-round-data))
    )
)


;; Process single ticket matches and update data
(define-private (process-ticket (ticket-info {user: principal, ticket-id: uint}))
    (let
        ((round-id (var-get current-round-id))
         (ticket (unwrap-panic (map-get? user-tickets {round-id: round-id, user: (get user ticket-info), ticket-id: (get ticket-id ticket-info)})))
         (winning-numbers (unwrap-panic (var-get drawn-number)))
         (matches (count-matching-numbers (get numbers ticket) winning-numbers))
         (current-round (unwrap-panic (map-get? lottery-rounds round-id)))
         (match-updates (if (>= matches u2)
            (unwrap-panic (element-at 
                (list 
                    {match-4-count: (+ (get match-4-count current-round) u1),
                     match-3-count: (get match-3-count current-round),
                     match-2-count: (get match-2-count current-round)}
                    {match-4-count: (get match-4-count current-round),
                     match-3-count: (+ (get match-3-count current-round) u1),
                     match-2-count: (get match-2-count current-round)}
                    {match-4-count: (get match-4-count current-round),
                     match-3-count: (get match-3-count current-round), 
                     match-2-count: (+ (get match-2-count current-round) u1)})
                (- u4 matches)))
            current-round))
         (updated-round (merge current-round match-updates))
         (prize-amount (calculate-prize-amount 
                        matches
                        (get total-prize updated-round)
                        {match-4-count: (get match-4-count updated-round),
                         match-3-count: (get match-3-count updated-round),
                         match-2-count: (get match-2-count updated-round)})))

        ;; Update number of winners for each prize tier in this round
        (map-set lottery-rounds round-id updated-round)

        ;; Update user ticket with matches and prize amount
        (map-set user-tickets
            {round-id: round-id, user: (get user ticket-info), ticket-id: (get ticket-id ticket-info)}
            {numbers: (get numbers ticket),
             matches: (some matches),
             claimed: false,
             prize-amount: (if (> prize-amount u0) 
                             (some prize-amount)
                             none)})
        (ok matches)))

(define-private (draw-numbers)
    (let
        ((current-block-height (- stacks-block-height u1))
         (tx-hash (unwrap! (get-stacks-block-info? id-header-hash current-block-height) err-can-not-get-hash))
         (seed (get-random-4-bytes tx-hash))
         (lottery-numbers (pick-four-numbers (unwrap-panic (as-max-len? seed u4))))
         (current-round (unwrap! (map-get? lottery-rounds (var-get current-round-id)) err-round-not-found))
         (total-tickets (get total-tickets current-round))
         (total-prize-pool (get total-prize current-round))
         (winning-numbers (list 
            (get first lottery-numbers)
            (get second lottery-numbers)
            (get third lottery-numbers)
            (get fourth lottery-numbers))))
        
        (var-set drawn-number (some winning-numbers))
        
        (let ((ticket-infos (unwrap-panic (map-get? round-ticket-ids (var-get current-round-id)))))
                (map process-ticket ticket-infos))
        
        (unwrap! (end-current-round) err-can-not-end-round)
        (ok true)))


(define-private (process-buy-ticket (numbers (list 4 uint)))
    (let (
        (round-id (var-get current-round-id))
        (ticket-id (var-get ticket-counter))
        (current-tickets (default-to (list) (map-get? round-ticket-ids round-id)))
    )
    ;; Save ticket information
    (map-set user-tickets
        {round-id: round-id, user: tx-sender, ticket-id: ticket-id}
        {
            numbers: numbers,
            matches: none,
            claimed: false,
            prize-amount: none
        })
    ;; Add ticket to round's ticket list
    (map-set round-ticket-ids 
        round-id
        (unwrap! (as-max-len? (append current-tickets {user: tx-sender, ticket-id: ticket-id}) u7) err-append-ticket-ids))
    ;; Increment counter
    (var-set ticket-counter (+ ticket-id u1))
    (ok {ticket-id: ticket-id, numbers: numbers})))

;; Function to buy multiple lottery tickets for current round
(define-public (buy-tickets (numbers-list (list 50 (list 4 uint))) (round-id uint))
    (let (
        (round (unwrap! (map-get? lottery-rounds round-id) err-round-not-found))
        (user-count (default-to u0 (map-get? user-ticket-count {round-id: round-id, user: tx-sender})))
        (tickets-to-buy (len numbers-list))
        (total-cost (* (- STX_LOTTERY_TICKET_PRICE STX_LOTTERY_FEE) tickets-to-buy))
        (total-fee (* STX_LOTTERY_FEE tickets-to-buy))
    )
    ;; Check if round is still active
    (asserts! (not (get is-ended round)) err-round-ended)
    ;; Check if round ID is valid
    (asserts! (>= round-id (var-get current-round-id)) err-round-ended)
    ;; Check if number of tickets doesn't exceed limit
    (asserts! (<= (+ user-count tickets-to-buy) MAX_TICKETS_PER_USER) err-too-many-tickets)
    
    ;; Transfer all funds at once
    (try! (stx-transfer? total-cost tx-sender contract-principal))
    (try! (as-contract (stx-transfer? total-fee tx-sender STX_LOTTERY_FEE_WALLET)))
    
    ;; Process ticket purchases and collect ticket IDs
    (let ((ticket-results (map process-buy-ticket numbers-list)))
    
        ;; Update user ticket count
        (map-set user-ticket-count 
            {round-id: round-id, user: tx-sender} 
            (+ user-count tickets-to-buy))
        
        ;; Update round information
        (let ((new-total-tickets (+ (get total-tickets round) tickets-to-buy))
              (new-total-prize (+ (get total-prize round) total-cost)))
            (map-set lottery-rounds round-id 
                (merge round {
                    total-tickets: new-total-tickets,
                    total-prize: new-total-prize
                }))
            
            ;; Draw numbers if threshold reached
            (begin
                (if (>= new-total-tickets DRAW_TICKET_THRESHOLD)
                    (begin
                        (try! (draw-numbers))
                        (try! (start-new-round)))
                    true)
                (ok ticket-results)))))
)

;; End current lottery round
(define-private (end-current-round) 
    (let
        ((current-round (unwrap-panic (map-get? lottery-rounds (var-get current-round-id))))
         (total-prize-pool (get total-prize current-round))
         (match-4-count (get match-4-count current-round))
         (match-3-count (get match-3-count current-round))
         (match-2-count (get match-2-count current-round))
         (match-4-pool (/ (* total-prize-pool MATCH_4_PRIZE_PERCENT) u100))
         (match-3-pool (/ (* total-prize-pool MATCH_3_PRIZE_PERCENT) u100))
         (match-2-pool (/ (* total-prize-pool MATCH_2_PRIZE_PERCENT) u100))
         (total-claimed-prize (+ 
            (if (> match-4-count u0) match-4-pool u0)
            (if (> match-3-count u0) match-3-pool u0)
            (if (> match-2-count u0) match-2-pool u0)
         ))
         (no-winner-prize (- total-prize-pool total-claimed-prize)))

        (map-set lottery-rounds (var-get current-round-id)
            (merge current-round {
                winning-numbers: (var-get drawn-number),
                is-ended: true,
                no-winner-prize: no-winner-prize,
                unclaimed-prize: total-prize-pool,
                end-block: stacks-block-height
            })
        )
        (var-set drawn-number none)
        (ok true)
))

;; Start new lottery round
(define-private (start-new-round)
    (let 
        (
         (new-round-id (+ (var-get current-round-id) u1))
         (previous-round (unwrap! (map-get? lottery-rounds (var-get current-round-id)) err-round-not-found))
         )
        (var-set current-round-id new-round-id)
        (var-set ticket-counter u1)
        (map-set lottery-rounds new-round-id {
            total-tickets: u0,
            winning-numbers: none,
            is-ended: false,
            total-prize: (get no-winner-prize previous-round),
            no-winner-prize: u0,
            unclaimed-prize: u0,
            match-4-count: u0,
            match-3-count: u0,
            match-2-count: u0,
            start-block: stacks-block-height,
            end-block: u0,
        })
        (ok true)
    )
)

;; Claim funds from contract wallet
(define-public (claim-funds)
    (let 
        ((caller tx-sender))
        (asserts! (is-eq caller (var-get contract-owner)) err-not-owner)
        (let ((balance (stx-get-balance contract-principal)))
            (as-contract 
                (stx-transfer? balance contract-principal (var-get contract-owner))
            )
        )
    )
)

;; transfer ownership
(define-public (transfer-ownership (new-owner principal))
  (begin
    ;; Checks if the sender is the current owner
    (if (is-eq tx-sender (var-get contract-owner))
      (begin
        ;; Sets the new owner
        (var-set contract-owner new-owner)
        ;; Returns success message
        (ok "Ownership transferred successfully"))
      ;; Error if the sender is not the owner
      (err err-not-owner)))
)

(define-private (is-round-expired (round-id uint))
    (let ((round (unwrap! (map-get? lottery-rounds round-id) false)))
        (let ((end-block-info (unwrap! (get-stacks-block-info? time (get end-block round)) false))
              (current-block-info (unwrap! (get-stacks-block-info? time stacks-block-height) false)))
            (> (- current-block-info end-block-info) CLAIM_EXPIRE_TIME))
    )
)
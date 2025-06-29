;; Layer 2 Payment Channel Network - Commit 1: Core Infrastructure
;; A simplified payment channel network for micro-transactions

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-not-authorized (err u101))
(define-constant err-channel-exists (err u102))
(define-constant err-channel-not-found (err u103))
(define-constant err-insufficient-funds (err u104))
(define-constant err-channel-closed (err u105))
(define-constant err-invalid-amount (err u109))
(define-constant err-invalid-parameters (err u121))
(define-constant err-not-registered (err u120))
(define-constant err-already-registered (err u119))
(define-constant err-self-payment (err u117))

;; State variables
(define-data-var next-channel-id uint u1)
(define-data-var min-channel-deposit uint u1000000) ;; 0.01 STX
(define-data-var protocol-fee-percentage uint u20) ;; 0.2%
(define-data-var protocol-fee-balance uint u0)

;; Participant registry
(define-map participants
  { participant: principal }
  {
    active: bool,
    total-channels: uint,
    reputation-score: uint
  }
)

;; Channel structure
(define-map channels
  { channel-id: uint }
  {
    participant1: principal,
    participant2: principal,
    capacity: uint,
    participant1-balance: uint,
    participant2-balance: uint,
    state: uint, ;; 0 = Open, 1 = Closing, 2 = Settled
    open-block: uint
  }
)

;; Channel lookup by participants
(define-map participant-channels
  { participant1: principal, participant2: principal }
  { channel-id: uint }
)

;; Initialize the contract
(define-public (initialize)
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (ok true)
  )
)

;; Register as network participant
(define-public (register-participant)
  (let (
    (participant tx-sender)
    (existing-record (map-get? participants { participant: participant }))
  )
    (asserts! (is-none existing-record) err-already-registered)
    
    (map-set participants
      { participant: participant }
      {
        active: true,
        total-channels: u0,
        reputation-score: u70
      }
    )
    
    (ok true)
  )
)

;; Open a new payment channel
(define-public (open-channel (participant2 principal) (deposit1 uint))
  (let (
    (participant1 tx-sender)
    (channel-id (var-get next-channel-id))
  )
    ;; Validations
    (asserts! (not (is-eq participant1 participant2)) err-self-payment)
    (asserts! (>= deposit1 (var-get min-channel-deposit)) err-invalid-amount)
    (asserts! (is-none (map-get? participant-channels { 
      participant1: participant1, 
      participant2: participant2 
    })) err-channel-exists)
    (asserts! (is-some (map-get? participants { participant: participant1 })) err-not-registered)
    
    ;; Transfer funds
    (try! (stx-transfer? deposit1 participant1 (as-contract tx-sender)))
    
    ;; Create channel
    (map-set channels
      { channel-id: channel-id }
      {
        participant1: participant1,
        participant2: participant2,
        capacity: deposit1,
        participant1-balance: deposit1,
        participant2-balance: u0,
        state: u0,
        open-block: block-height
      }
    )
    
    ;; Create lookups
    (map-set participant-channels
      { participant1: participant1, participant2: participant2 }
      { channel-id: channel-id }
    )
    (map-set participant-channels
      { participant1: participant2, participant2: participant1 }
      { channel-id: channel-id }
    )
    
    ;; Update state
    (var-set next-channel-id (+ channel-id u1))
    
    (ok channel-id)
  )
)

;; Join an existing channel
(define-public (join-channel (channel-id uint) (deposit uint))
  (let (
    (participant tx-sender)
    (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
  )
    ;; Validations
    (asserts! (is-eq (get state channel) u0) err-channel-closed)
    (asserts! (is-eq participant (get participant2 channel)) err-not-authorized)
    (asserts! (is-eq (get participant2-balance channel) u0) err-invalid-parameters)
    (asserts! (>= deposit (var-get min-channel-deposit)) err-invalid-amount)
    
    ;; Transfer funds
    (try! (stx-transfer? deposit participant (as-contract tx-sender)))
    
    ;; Update channel
    (map-set channels
      { channel-id: channel-id }
      (merge channel {
        capacity: (+ (get capacity channel) deposit),
        participant2-balance: deposit
      })
    )
    
    (ok true)
  )
)

;; Make a payment within a channel
(define-public (make-payment (channel-id uint) (amount uint))
  (let (
    (sender tx-sender)
    (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
    (participant1 (get participant1 channel))
    (participant2 (get participant2 channel))
    (is-sender-participant1 (is-eq sender participant1))
  )
    ;; Validations
    (asserts! (is-eq (get state channel) u0) err-channel-closed)
    (asserts! (or is-sender-participant1 (is-eq sender participant2)) err-not-authorized)
    
    ;; Check balance
    (let (
      (sender-balance (if is-sender-participant1 
                         (get participant1-balance channel)
                         (get participant2-balance channel)))
    )
      (asserts! (>= sender-balance amount) err-insufficient-funds)
      
      ;; Update balances
      (map-set channels
        { channel-id: channel-id }
        (merge channel 
          (if is-sender-participant1
            { 
              participant1-balance: (- (get participant1-balance channel) amount),
              participant2-balance: (+ (get participant2-balance channel) amount)
            }
            { 
              participant1-balance: (+ (get participant1-balance channel) amount),
              participant2-balance: (- (get participant2-balance channel) amount)
            }
          )
        )
      )
      
      (ok true)
    )
  )
)

;; Close channel cooperatively
(define-public (close-channel (channel-id uint))
  (let (
    (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
    (participant1 (get participant1 channel))
    (participant2 (get participant2 channel))
    (balance1 (get participant1-balance channel))
    (balance2 (get participant2-balance channel))
  )
    ;; Validations
    (asserts! (is-eq (get state channel) u0) err-channel-closed)
    (asserts! (or (is-eq tx-sender participant1) (is-eq tx-sender participant2)) err-not-authorized)
    
    ;; Calculate fees
    (let (
      (fee1 (/ (* balance1 (var-get protocol-fee-percentage)) u10000))
      (fee2 (/ (* balance2 (var-get protocol-fee-percentage)) u10000))
      (final-balance1 (- balance1 fee1))
      (final-balance2 (- balance2 fee2))
    )
      ;; Transfer balances
      (as-contract (try! (stx-transfer? final-balance1 (as-contract tx-sender) participant1)))
      (as-contract (try! (stx-transfer? final-balance2 (as-contract tx-sender) participant2)))
      
      ;; Update fee balance
      (var-set protocol-fee-balance (+ (var-get protocol-fee-balance) (+ fee1 fee2)))
      
      ;; Update channel state
      (map-set channels
        { channel-id: channel-id }
        (merge channel {
          state: u2,
          participant1-balance: u0,
          participant2-balance: u0,
          capacity: u0
        })
      )
      
      (ok { 
        participant1-balance: final-balance1, 
        participant2-balance: final-balance2 
      })
    )
  )
)

;; HTLC Support - Commit 2: Hashed Time-Locked Contracts

;; Additional error constants for HTLCs
(define-constant err-invalid-htlc (err u113))
(define-constant err-htlc-expired (err u114))
(define-constant err-incorrect-preimage (err u115))
(define-constant err-invalid-state (err u108))

;; Additional state variables
(define-data-var next-htlc-id uint u1)

;; HTLC structure for conditional payments
(define-map htlcs
  { htlc-id: uint }
  {
    channel-id: uint,
    sender: principal,
    receiver: principal,
    amount: uint,
    hashlock: (buff 32),
    timelock: uint,
    preimage: (optional (buff 32)),
    claimed: bool,
    refunded: bool,
    created-at: uint
  }
)

;; Read-only functions
(define-read-only (get-channel-info (channel-id uint))
  (map-get? channels { channel-id: channel-id })
)

(define-read-only (get-participant-info (participant principal))
  (map-get? participants { participant: participant })
)

(define-read-only (get-network-stats)
  {
    total-channels: (- (var-get next-channel-id) u1),
    protocol-fee: (var-get protocol-fee-percentage),
    min-deposit: (var-get min-channel-deposit),
    protocol-fee-balance: (var-get protocol-fee-balance)
  }
)

;; Admin functions
(define-public (update-protocol-fee (new-fee uint))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (asserts! (<= new-fee u1000) err-invalid-parameters)
    (var-set protocol-fee-percentage new-fee)
    (ok true)
  )
)

(define-public (withdraw-protocol-fees (recipient principal))
  (begin
    (asserts! (is-eq tx-sender contract-owner) err-owner-only)
    (let ((fee-amount (var-get protocol-fee-balance)))
      (asserts! (> fee-amount u0) err-insufficient-funds)
      (var-set protocol-fee-balance u0)
      (as-contract (try! (stx-transfer? fee-amount (as-contract tx-sender) recipient)))
      (ok fee-amount)
    )
  )
)

;; Create a Hashed Time-Locked Contract (HTLC)
(define-public (create-htlc 
  (channel-id uint)
  (receiver principal)
  (amount uint)
  (hashlock (buff 32))
  (timelock uint))
  
  (let (
    (sender tx-sender)
    (htlc-id (var-get next-htlc-id))
    (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
    (participant1 (get participant1 channel))
    (participant2 (get participant2 channel))
    (is-participant1 (is-eq sender participant1))
  )
    ;; Validations
    (asserts! (is-eq (get state channel) u0) err-channel-closed)
    (asserts! (or is-participant1 (is-eq sender participant2)) err-not-authorized)
    (asserts! (if is-participant1 
               (is-eq receiver participant2)
               (is-eq receiver participant1)) 
             err-not-authorized)
    (asserts! (> timelock block-height) err-invalid-parameters)
    
    ;; Check balance
    (let (
      (sender-balance (if is-participant1 
                         (get participant1-balance channel)
                         (get participant2-balance channel)))
    )
      (asserts! (>= sender-balance amount) err-insufficient-funds)
      
      ;; Create HTLC
      (map-set htlcs
        { htlc-id: htlc-id }
        {
          channel-id: channel-id,
          sender: sender,
          receiver: receiver,
          amount: amount,
          hashlock: hashlock,
          timelock: timelock,
          preimage: none,
          claimed: false,
          refunded: false,
          created-at: block-height
        }
      )
      
      ;; Lock sender funds
      (if is-participant1
        (map-set channels
          { channel-id: channel-id }
          (merge channel { participant1-balance: (- (get participant1-balance channel) amount) })
        )
        (map-set channels
          { channel-id: channel-id }
          (merge channel { participant2-balance: (- (get participant2-balance channel) amount) })
        )
      )
      
      ;; Update HTLC ID
      (var-set next-htlc-id (+ htlc-id u1))
      
      (ok htlc-id)
    )
  )
)

;; Fulfill HTLC with preimage
(define-public (fulfill-htlc (htlc-id uint) (preimage (buff 32)))
  (let (
    (receiver tx-sender)
    (htlc (unwrap! (map-get? htlcs { htlc-id: htlc-id }) err-invalid-htlc))
    (channel-id (get channel-id htlc))
    (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
  )
    ;; Validations
    (asserts! (not (get claimed htlc)) err-invalid-state)
    (asserts! (not (get refunded htlc)) err-invalid-state)
    (asserts! (< block-height (get timelock htlc)) err-htlc-expired)
    (asserts! (is-eq receiver (get receiver htlc)) err-not-authorized)
    (asserts! (is-eq (sha256 preimage) (get hashlock htlc)) err-incorrect-preimage)
    
    ;; Credit receiver
    (let (
      (amount (get amount htlc))
      (participant1 (get participant1 channel))
      (is-receiver-participant1 (is-eq receiver participant1))
    )
      ;; Update receiver balance
      (if is-receiver-participant1
        (map-set channels
          { channel-id: channel-id }
          (merge channel { participant1-balance: (+ (get participant1-balance channel) amount) })
        )
        (map-set channels
          { channel-id: channel-id }
          (merge channel { participant2-balance: (+ (get participant2-balance channel) amount) })
        )
      )
      
      ;; Update HTLC
      (map-set htlcs
        { htlc-id: htlc-id }
        (merge htlc {
          preimage: (some preimage),
          claimed: true
        })
      )
      
      (ok true)
    )
  )
)

;; Refund expired HTLC
(define-public (refund-htlc (htlc-id uint))
  (let (
    (sender tx-sender)
    (htlc (unwrap! (map-get? htlcs { htlc-id: htlc-id }) err-invalid-htlc))
    (channel-id (get channel-id htlc))
    (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
  )
    ;; Validations
    (asserts! (not (get claimed htlc)) err-invalid-state)
    (asserts! (not (get refunded htlc)) err-invalid-state)
    (asserts! (>= block-height (get timelock htlc)) err-htlc-expired)
    (asserts! (is-eq sender (get sender htlc)) err-not-authorized)
    
    ;; Refund sender
    (let (
      (amount (get amount htlc))
      (participant1 (get participant1 channel))
      (is-sender-participant1 (is-eq sender participant1))
    )
      ;; Update sender balance
      (if is-sender-participant1
        (map-set channels
          { channel-id: channel-id }
          (merge channel { participant1-balance: (+ (get participant1-balance channel) amount) })
        )
        (map-set channels
          { channel-id: channel-id }
          (merge channel { participant2-balance: (+ (get participant2-balance channel) amount) })
        )
      )
      
      ;; Update HTLC
      (map-set htlcs
        { htlc-id: htlc-id }
        (merge htlc { refunded: true })
      )
      
      (ok true)
    )
  )
)

;; Get HTLC information
(define-read-only (get-htlc-info (htlc-id uint))
  (map-get? htlcs { htlc-id: htlc-id })
)

;; Multi-hop Routing - Commit 3: Route Finding and Multi-hop Payments

;; Additional error constants for routing
(define-constant err-route-not-found (err u116))
(define-constant err-invalid-route (err u110))

;; Simple routing function - finds direct route or 1-hop route
(define-public (find-route (sender principal) (receiver principal) (amount uint))
  (let (
    (direct-channel (map-get? participant-channels { participant1: sender, participant2: receiver }))
  )
    (if (is-some direct-channel)
      ;; Direct route exists
      (let (
        (channel-id (get channel-id (unwrap-panic direct-channel)))
        (channel (unwrap-panic (map-get? channels { channel-id: channel-id })))
        (sender-balance (if (is-eq sender (get participant1 channel))
                           (get participant1-balance channel)
                           (get participant2-balance channel)))
      )
        (if (>= sender-balance amount)
          (ok (list channel-id))
          (err err-insufficient-funds)
        )
      )
      ;; No direct route - return error (for simplicity)
      (err err-route-not-found)
    )
  )
)

;; Start multi-hop payment using HTLC
(define-public (start-multi-hop-payment 
  (receiver principal)
  (amount uint)
  (route (list 10 uint))
  (secret (buff 32)))
  
  (let (
    (sender tx-sender)
    (hashlock (sha256 secret))
    (timelock (+ block-height u144)) ;; 1 day timelock
    (route-length (len route))
  )
    ;; Validate route
    (asserts! (> route-length u0) err-invalid-route)
    
    ;; For single channel route
    (if (is-eq route-length u1)
      (let (
        (channel-id (unwrap-panic (element-at route u0)))
        (channel (unwrap! (map-get? channels { channel-id: channel-id }) err-channel-not-found))
      )
        ;; Verify this is a valid route
        (asserts! (or 
                   (and (is-eq sender (get participant1 channel)) (is-eq receiver (get participant2 channel)))
                   (and (is-eq sender (get participant2 channel)) (is-eq receiver (get participant1 channel)))
                  ) 
                  err-invalid-route)
        
        ;; Create HTLC
        (let ((htlc-result (try! (create-htlc channel-id receiver amount hashlock timelock))))
          (ok {
            hashlock: hashlock,
            timelock: timelock,
            htlc-id: htlc-result
          })
        )
      )
      ;; Multi-hop not implemented in this simplified version
      (err err-invalid-route)
    )
  )
)

;; Complete multi-hop payment by revealing preimage
(define-public (complete-multi-hop-payment (htlc-id uint) (preimage (buff 32)))
  (begin
    (try! (fulfill-htlc htlc-id preimage))
    (ok { preimage: preimage })
  )
)


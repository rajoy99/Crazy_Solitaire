#lang racket

;; Spades, clubs, Hearts, Diamonds
;; Coding cards
(require racket/list)  ;; shuffle

;; F1 - The cards
;; Invariant representation
;; a pair of letter


;; to check whether it is a valid card or not. Returns true if it is a valid card; false otherwise

(define (card? card)
  (cond
    [(not (cons? card)) false]
    [(and (not (number? (car card))) (not (or (equal? #\K (car card)) (equal? #\Q (car card)) (equal? #\J (car card))))) false]
    [(and (number? (car card)) (or (< (car card) 1) (> (car card) 7))) false]
    [(not (or (equal? #\H (cdr card)) (equal? #\S (cdr card)) (equal? #\C (cdr card)) (equal? #\D (cdr card)))) false]
    [else true]
  )
)


(define (suit val)
  (cdr val)
  )

(define (numeral val)
  (car val)
  )

;; checking whether it is a face card or not 
(define (face? card)
  (cond
    [(or (equal? #\K (car card)) (equal? #\Q (car card)) (equal? #\J (car card))) true]
    [else false]
  )
)

;; prints card contents to string format 
(define (card->string card)
  (cond
    [(face? card) (list->string (list (car card) (cdr card)))]
    [else (list->string (list (integer->char (+ 48 (car card))) (cdr card)))]
  )
)

;; obtain the value of card according to the rules of the game
(define (value card)
  (cond
    [(face? card) 0.5]
    [else (car card)]
    
  )
)


;; 3 .- The deck.
;; to check whether it is a valid deck or not. Returns true if it is a valid deck; false otherwise
(define (deck? deck)
  (cond
    [(not (list? deck)) false]
    [(empty? deck) true]
    [(card? (first deck)) (deck? (rest deck))]
    [else false]
  )
)

;; calculate the value of the deck by recursive calls 
(define (valueOf deck)
  (cond
    [(empty? deck) 0]
    [else (+ (value (first deck)) (valueOf (rest deck)))]
  )
)

;; hard
;; given a valid suit (#\C, #\H, #\D,#\S) returns the whole series for it
(define (do-suite char)
  (list (cons 1 char) (cons 2 char) (cons 3 char) (cons 4 char) (cons 5 char) (cons 6 char) (cons 7 char) (cons #\J char) (cons #\Q char) (cons #\K char))
)
          
;; how to generate a deck.

(define deck (append (do-suite #\H) (do-suite #\S) (do-suite #\C) (do-suite #\D)))

(define (deck->strings deck)
  (cond
    [(empty? deck) (list )]
    [else (append (list (card->string (first deck))) (deck->strings (rest deck)))]
  )
)


;;(define (playS deck hand strategy)
;;F3 - Probabilities.

;;; (define (probability comp num deck) 0)
(define (length lst)
  (cond
    [(empty? lst)  0]
    [(cons? lst)   (+ 1 (length (rest lst)))]))


(define (optionfilter operator lst num)
  (length (filter (lambda (x) (and (number? x) (operator x num)))
          lst)))

(define (dictarraytolist val)
  (map value val))


(define (probability operator num cardlist)
  (define clist (dictarraytolist cardlist))
  (optionfilter operator clist num)


  )

(define cheat #f)



;; F4.- Game.
;; DO NO CHANGE THE  FUNCTIONS BELOW THIS LINE
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
;; -----------------------------------------------------
(define (show-statistics deck hand)
  (let
    ([toCheck (- 7.5 (valueOf hand))])
    (display
     (format
      "P(>7.5):~a/~a\nP(<7.5):~a/~a\nP(=7.5):~a/~a\nHAND:~a~nVALUE:~a\nDECK:~a...\n"
      (probability > toCheck deck)
      (length deck)
      (probability < toCheck deck)
      (length deck)
      (probability = toCheck deck)
      (length deck)                     
      (deck->strings hand)
      (valueOf hand)
      (if cheat (deck->strings (take deck
                                   (max 0 
                                    (min 4 (length deck) )))) "****")
      )
     )))
  
;; Human interaction.
(define (play deck hand)
  (begin      
      (show-statistics deck hand)
      ;; Control
      (cond
      [(= (valueOf hand) 7.5) (display "WIN")]
      [(> (valueOf hand) 7.5) (display "LOST")]
      [(empty? deck) (display "NO CARDS LEFT") ]
      [(let
           ([ command (read)])
           (cond
             [(equal? command 'accept)
               (play (rest deck) (cons (first deck) hand))]
             [(equal? command 'pass)
               (play (drop deck 1) hand)]
             [(equal? command 'end) (void)]
             [else (play deck hand)]))])))

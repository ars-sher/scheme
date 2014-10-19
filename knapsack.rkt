#lang scheme/base

; call from the interpreter:
; !racket %:p

; call example: (main '(1 1 2 2) '(4 3 2 1) 3 4)
(define (main weights costs B K)
  (let ( (result (knapsack weights costs B)) )
    (if (>= (list-ref result 1) K)
      (begin
        (print #t)
        (newline)
        (print (list-ref result 0))
        (newline)
        (print (list-ref result 1))
        (newline)
        (print (list-ref result 2))
      )
      (print #f)
    )
  )
)


(define (SIZE) 5)
(define (CROSSOVER_PERCENT) 0.7)

; returns (sum_weight sum_cost list_of_item_indexes)
(define (knapsack weights costs B)
  (let ( (N (length weights)) )
    ; returns list of binary chromosomes
    (define (initialize-population)
      ; generate binary list of length N
      (define (gen-bin-list lst)
        (if (= N (length lst))
          lst
          (gen-bin-list (cons (random 2) lst))
        )
      )
      (define (gen-population lst)
        (if (= (SIZE) (length lst))
          lst
          (gen-population (cons (gen-bin-list '()) lst))
        )
      )
      (gen-population '())
    )

    (define (knapsack-cycle population)
      population
    )

    (print (knapsack-cycle (initialize-population)))
    (newline)
    '(2 7 (1 2))
  )
)

(main '(1 1 2 2) '(4 3 2 1) 3 4)


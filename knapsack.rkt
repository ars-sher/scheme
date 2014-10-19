#lang scheme/base

;call example: (main '(1 1 2 2) '(4 3 2 1) 3 4)
(define (main weights costs B K)
  (define (N)
    (length weights)
  )
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

;returns (sum_weight sum_cost list_of_item_indexes)
(define (knapsack weights costs B)
  '(2 7 (1 2))
)


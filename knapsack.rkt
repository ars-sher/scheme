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


; returns (sum_weight sum_cost list_of_item_indexes)
(define (knapsack weights costs B)
  (let
    ( 
      (N (length weights))
      (SIZE 5)
      (CROSSOVER_PERCENT 0.7)
      (GENS_NUMB_LIMIT 1)
    )
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
        (if (= SIZE (length lst))
          lst
          (gen-population (cons (gen-bin-list '()) lst))
        )
      )
      (gen-population '())
    )

    ; returns a pair of solution and its fitness
    (define (solution-fitness chromosome)
      (define (calc-weight)
        (let ( (chromosomes-weights (zip chromosome weights)) )
          (foldl 
            (lambda (x old)
              (if (= 1 (car x))
                (+ old (cdr x))
                old
              )
            )
            0
            chromosomes-weights
          )
        )
      )

      (cons chromosome (calc-weight))
    )

    (define (knapsack-cycle population generation-numb)
      (if (> generation-numb GENS_NUMB_LIMIT) 
        population
        (let ( (solutions-fitnesses (map solution-fitness population)) )
          ; (map (lambda (x) (car x)) solutions-fitnesses)
          solutions-fitnesses
        )
      )
    )

    (print (knapsack-cycle (initialize-population) 0))
    (newline)
    '(2 7 (1 2))
  )
)

(define (zip lst1 lst2)
  (map cons lst1 lst2)
)

(main '(1 1 2 2) '(4 3 2 1) 3 4)


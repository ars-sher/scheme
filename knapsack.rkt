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
      (TOURNAMENT_PARTICIPANTS 2)
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
      (define (calc-weight-or-fit x)
        (let ( (chromosomes-weights (zip chromosome x)) )
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

      ; drops random item from chromosome
      (define (drop-random-item)
        (let ( (pos (random N)) )
          (let
            (
              (head (take chromosome pos))
              (tail (list-tail chromosome pos))
            )
            (append head (cons 0 (cdr tail)))
          )
        )
      )

      (if (> (calc-weight-or-fit weights) B)
        (solution-fitness (drop-random-item))
        (cons chromosome (calc-weight-or-fit costs))
      )
    )

    ; chooses SIZE parents
    (define (choose-parents solutions-fitnesses)
      (define (choose-parents-cycle res res-size)
        (if (= res-size SIZE)
          res
          (choose-parents-cycle (cons (run-tournament) res) (+ res-size 1))
        )
      )

      ; chooses one parent in tournament
      (define (run-tournament)
        (define (run-tournament-cycle res res-size)
          (if (= res-size TOURNAMENT_PARTICIPANTS)
            ;TODO
            (car (max res (lambda (x) (cdr x)))) 
            (run-tournament-cycle (cons (list-ref solutions-fitnesses (random SIZE)) res) (+ res-size 1))
          )
        )

        (run-tournament-cycle '() 0)
      )

      (choose-parents-cycle '() 0)
    )

    (define (knapsack-cycle population generation-numb)
      (if (> generation-numb GENS_NUMB_LIMIT) 
        population
        (let ( (solutions-fitnesses (map solution-fitness population)) )
          (let ( (parents (choose-parents solutions-fitnesses)) )
            ; (map (lambda (x) (car x)) solutions-fitnesses)
            (cons solutions-fitnesses parents)
          )
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

(define (take lst pos)
  (define (take-cycle lst res n)
    (if (= n pos)
      res
      (take-cycle (cdr lst) (cons (car lst) res) (+ n 1))
    )
  )
  (reverse (take-cycle lst '() 0))
)

; returns max element (defined by function f, f returns non-negative numbers) from non-empty list
(define (max lst f)
  (define (max-loop lst res)
    (cond
      ((null? lst) res)
      ((> (f (car lst)) (f res)) (max-loop (cdr lst) (car lst)))
      (else (max-loop (cdr lst) res))
    )
  )

  (max-loop (cdr lst) (car lst))
)

(main '(1 1 2 2) '(4 3 2 1) 3 4)
;(max '(1 2 3) (lambda (x) (modulo x 2)))

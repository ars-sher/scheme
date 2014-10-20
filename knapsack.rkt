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
      (MUTATION_PROBABILITY (* 1.0 (/ 1 (length weights))))
      (SIZE 10)
      (CROSSOVER_PERCENT 0.7)
      (GENS_NUMB_LIMIT 10)
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

    ; calculates weight of fitness of chromosome
    (define (calc-weight-or-fit chromosome x)
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

    ; returns a pair of solution and its fitness
    (define (solution-fitness chromosome)
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

      (if (> (calc-weight-or-fit chromosome weights) B)
        (solution-fitness (drop-random-item))
        (cons chromosome (calc-weight-or-fit chromosome costs))
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
            (car (max res (lambda (x) (cdr x)))) 
            (run-tournament-cycle (cons (list-ref solutions-fitnesses (random SIZE)) res) (+ res-size 1))
          )
        )

        (run-tournament-cycle '() 0)
      )

      (choose-parents-cycle '() 0)
    )

    ; combines parents and makes crossover, giving birth to new generation
    (define (give-birth parents)
      (define (crossover p1 p2)
        (if (> (random) CROSSOVER_PERCENT)
          (cons p1 p2)
          (let ( (pos (quotient N 2)) )
            (let
              (
                (head1 (take p1 pos))
                (head2 (take p2 pos))
                (tail1 (list-tail p1 pos))
                (tail2 (list-tail p2 pos))
              )
              (cons (append head1 tail2) (append head2 tail1))
            )
          )
        )
      )

      (define (unite lst)
        (define (unite-loop lst res)
          (if (null? lst)
            res
            (unite-loop (cdr lst) (cons (caar lst) (cons (cdar lst) res)))
          )
        )

        (unite-loop lst '())
      )

      (let ( (pos (quotient SIZE 2)) )
        (let
          (
            (parents1 (take parents pos))
            (parents2 (list-tail parents pos))
          )
          (unite
            (map crossover parents1 parents2)
          )
        )
      )
    )

    (define (mutate chromosome)
      (map
        (lambda (x)
          (if (< (random) MUTATION_PROBABILITY)
            (modulo (+ x 1) 2)
            x
          )
        )
        chromosome
      )
    )

    (define (knapsack-cycle population generation-numb last-fitness fitness-stability)
      (let ( (solutions-fitnesses (map solution-fitness population)) )
        (let ( (best-solution (max solutions-fitnesses (lambda (x) (cdr x)))) )
          ;(if (> generation-numb GENS_NUMB_LIMIT) 
          (if (= fitness-stability 25) 
            (cons 
                (calc-weight-or-fit (car best-solution) weights)
                (cons (cdr best-solution) (cons (car best-solution) '()))
            )
            (let ( (parents (choose-parents solutions-fitnesses)) )
              (let ( (new-generation (give-birth parents)) )
                (let ( (new-generation-mutated (map mutate new-generation)) )
                  ;(cons solutions-fitnesses new-generation)
                  (knapsack-cycle 
                    new-generation-mutated 
                    (+ generation-numb 1)
                    (cdr best-solution)
                    (if (= last-fitness (cdr best-solution))
                      (+ 1 fitness-stability)
                      0
                    )
                  )
                )
              )
            )
          )
        )
      )
    )

    (knapsack-cycle (initialize-population) 0 0 0)
    ;(print (knapsack-cycle (initialize-population) 0))
    ;(newline)
    ;'(2 7 (1 2))
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

(define (flatten x)
  (cond
    ((null? x) '())
    ((not (pair? x)) (list x))
    (else (append (flatten (car x)) (flatten (cdr x))))
  )
)

(main '(1 1 2 2) '(4 3 2 1) 3 4)

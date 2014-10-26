#lang scheme/base

; call from the interpreter:
; !racket %:p

; call example: (main-genetic '(1 1 2 2) '(4 3 2 1) 3 4)

(define (main-genetic weights costs B K)
  (main knapsack weights costs B K)
)

(define (main-dummy weights costs B K)
  (main simple-solve weights costs B K)
)

(define (main f weights costs B K)
  (let ( (result (f weights costs B)) )
    (if (>= (list-ref result 1) K)
      (begin
        (let ( (answer (cons #t result)) )
          (for ([i (in-list answer)])
             (displayln i))
          answer
        )
      )
      (begin
        (print #f)
        (newline)
        (list #f)
      )
    )
  )
)


; returns (sum_weight sum_cost chromosome)
(define (knapsack weights costs B)
  (let
    ( 
      (N (length weights))
      (MUTATION_PROBABILITY (* 1.0 (/ 1 (length weights))))
      (SIZE 10)
      (CROSSOVER_PERCENT 0.7)
      (GENS_NUMB_LIMIT 10)
      (TOURNAMENT_PARTICIPANTS 2)
      (MAX_STABILITY 25)
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
        (let*
          (
            (pos (random N))
            (head (take chromosome pos))
            (tail (list-tail chromosome pos))
          )
          (append head (cons 0 (cdr tail)))
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

    (define (dbg generation-numb population solutions-fitnesses best-solution last-fitness new-stability)
      (printf "generation number: ~a\n" generation-numb)
      (printf "generation:\n") 
      (for ([i (in-list population)])
         (displayln i))
      (printf "solutions-fitnesses:\n") 
      (for ([i (in-list solutions-fitnesses)])
         (displayln i))     
      (printf "best solution: ~a\n" best-solution)
      (printf "last-fitness: ~a\n" last-fitness)
      (printf "new fitness-stability: ~a\n" new-stability)
    )

    (define (knapsack-cycle population generation-numb last-fitness fitness-stability)
      (let*
        ( 
          (solutions-fitnesses (map solution-fitness population)) 
          (best-solution (max solutions-fitnesses (lambda (x) (cdr x))))
          (new-stability
            (if (= last-fitness (cdr best-solution))
              (+ 1 fitness-stability)
              0
            )   
          )
        )
        ;(dbg generation-numb population solutions-fitnesses best-solution last-fitness new-stability)
        ;(if (> generation-numb GENS_NUMB_LIMIT) 
        (if (= new-stability MAX_STABILITY) 
          (cons 
              (calc-weight-or-fit (car best-solution) weights)
              (cons (cdr best-solution) (cons (car best-solution) '()))
          )
          (let*
            (
              (parents (choose-parents solutions-fitnesses))
              (new-generation (give-birth parents))
              (new-generation-mutated (map mutate new-generation))
            )
            ;(cons solutions-fitnesses new-generation)
            (knapsack-cycle 
              new-generation-mutated 
              (+ generation-numb 1)
              (cdr best-solution)
              new-stability  
            )
          )
        )
      )
    )

    (knapsack-cycle (initialize-population) 0 0 0)
  )
)

(define (simple-solve weights costs B)
  (define (calc-weight-or-fit chromosome x)
    (let ( (chromosomes-weights (zip chromosome x)) )
      (foldl 
        (lambda (x old)
          (if (equal? #t (car x))
            (+ old (cdr x))
            old
          )
        )
        0
        chromosomes-weights
      )
    )
  )

  (define (calc-weight vect)
    (calc-weight-or-fit vect weights)
  )

  (define (calc-fit vect)
    (calc-weight-or-fit vect costs)
  )

  ;format answer
  (define (return vect fit)
    (cons (calc-weight vect)
      (cons fit
        (cons vect '())
      )
    )
  )

  ;next candidate for solution
  (define (give-next v)
    (define (give-next-cycle watched vect)
      (cond
        ((null? vect) '())
        ((equal? (car vect) #f) (append watched (cons #t (cdr vect))))
        (else (give-next-cycle (cons #f watched) (cdr vect))) 
      )        
    )

    (give-next-cycle '() v)
  )

  (define (simple-solve-cycle curr-vect best-vect best-fit)
    (let ( (next-vect (give-next curr-vect)) )
      (cond
        ((null? next-vect) (return best-vect best-fit))
        (
          (and (<= (calc-weight next-vect) B) (> (calc-fit next-vect) best-fit))
          (simple-solve-cycle next-vect next-vect (calc-fit next-vect))
        )
        (else (simple-solve-cycle next-vect best-vect best-fit))
      )
    )
  )
  
  ;initialize brute-force
  (let ( (zeros (build-list (length weights) (lambda (x) #f))) )
    (simple-solve-cycle zeros zeros 0)
  )
)

(define (gen-test)
  (let
    (
      (FALSE_PROBABILITY 0.01)
    )
    ;generates uncorrelated test
    (define (gen-uncorrelated) 
      '((1 1 2 2) (4 3 2 1) 3)
    )

    (let*
      (
        (selector (random))
        (false-selector (random))
        (test
          (cond
            ((< selector 1) (gen-uncorrelated))
          )
        )
        (weights (list-ref test 0))
        (costs (list-ref test 1))
        (B (list-ref test 2))
        (solution (simple-solve weights costs B))  
        (max-fit (list-ref solution 1))
      )
      (if (<= false-selector FALSE_PROBABILITY)
        (cons (append-el test (* max-fit 2)) (list #f))
        (cons (append-el test (quotient max-fit 2)) (cons #t solution))
      )
    )
  )
)

;support functions
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

; appends element to the end of list
(define (append-el lst el)
  (reverse
    (cons el (reverse lst))
  )
)

;(main-genetic '(1 1 2 2) '(4 3 2 1) 3 4)
;(newline)
;(newline)
;(main-dummy '(1 1 2 2) '(4 3 2 1) 3 4)
(gen-test)

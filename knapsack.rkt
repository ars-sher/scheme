#lang scheme/base

; call from the terminal:
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
          ;(print-lst answer)
          answer
        )
      )
      (begin
        ;(print #f)
        ;(newline)
        (list #f)
      )
    )
  )
)


; returns (sum_weight sum_cost chromosome)
(define (knapsack weights costs B)
  (let*
    ( 
      (N (length weights))
      (MUTATION_PROBABILITY (* 1.0 (/ 1 (* 2 (length weights)))))
      (SIZE
        (if (<= N 8)
          (expt 2 N)
          500
        ) 
      )
      ;(SIZE 6)
      (CROSSOVER_PERCENT 0.7)
      (GENS_NUMB_LIMIT 4)
      (TOURNAMENT_PARTICIPANTS 2)
      (MAX_STABILITY 25)
      (ELITISM (min (percent-int SIZE 12) 15))
    )
    ; returns list of binary chromosomes size of SIZE + ELITISM
    (define (initialize-population)
      ; generate binary list of length N
      (define (gen-bin-list lst)
        (if (= N (length lst))
          lst
          (gen-bin-list (cons (= 1 (random 2)) lst))
        )
      )

      (define (gen-population lst)
        (if (= (+ SIZE ELITISM) (length lst))
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
          (append head (cons #f (cdr tail)))
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
            (not x)
            x
          )
        )
        chromosome
      )
    )

    (define (dbg generation-numb population solutions-fitnesses best-solution last-fitness new-stability elite)
      (printf "generation number: ~a\n" generation-numb)
      (printf "generation:\n") 
      (for ([i (in-list population)])
         (displayln i))
      (printf "solutions-fitnesses:\n") 
      (for ([i (in-list solutions-fitnesses)])
         (displayln i))     
      (print-lst elite "elite:")
      (printf "best solution: ~a\n" best-solution)
      (printf "last-fitness: ~a\n" last-fitness)
      (printf "new fitness-stability: ~a\n" new-stability)
    )

    (define (knapsack-cycle population generation-numb last-fitness fitness-stability)
      (define (sf-cmp sf1 sf2)
        (> (cdr sf1) (cdr sf2))
      )

      (let*
        ( 
          (solutions-fitnesses-with-elitism (map solution-fitness population)) 
          (solutions-fitnesses-sorted (sort solutions-fitnesses-with-elitism sf-cmp))
          ; drop ELITISM excess elements from the end
          (solutions-fitnesses (take solutions-fitnesses-sorted SIZE))
          (best-solution (car solutions-fitnesses))
          (new-stability
            (if (= last-fitness (cdr best-solution))
              (+ 1 fitness-stability)
              0
            )   
          )
          (elite (map (lambda (x) (car x)) (take solutions-fitnesses ELITISM)))
        )
        ;(dbg generation-numb population solutions-fitnesses best-solution last-fitness new-stability elite)
        ;(if (or (= new-stability MAX_STABILITY) (> generation-numb GENS_NUMB_LIMIT))
        ;(if (= new-stability MAX_STABILITY) 
        ;(if (> generation-numb GENS_NUMB_LIMIT) 
        (if (= new-stability MAX_STABILITY) 
          (list
            (calc-weight-or-fit (car best-solution) weights)
            (cdr best-solution)
            (binary-string-to-indexes (car best-solution))
          )
          (let*
            (
              (parents (choose-parents solutions-fitnesses))
              (new-generation (give-birth parents))
              (new-generation-mutated (map mutate new-generation))
              (ng-mutated-with-elitism (append new-generation-mutated elite))
            )
            (knapsack-cycle 
              ng-mutated-with-elitism 
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
    (list
      (calc-weight vect)
      fit
      (binary-string-to-indexes vect)
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
  (let*
    (
      (FALSE_PROBABILITY 0.5)
      (WEIGHT_MAX 100)
      ; for uncorrelated tests only
      (COST_MAX 100)
      (ITEMS_MAX 15)
      (STRONG_CORRELATING_CONST 10)
      (WEAKLY_CORR_NBH (percent-int WEIGHT_MAX 10))
      (items-numb (+ 1 (random ITEMS_MAX)))
      ;(items-numb 20)
      (W
	    (quotient
	      (+ 1 (random (* items-numb WEIGHT_MAX)))
		  (+ 1 (random 2))
		)
	  )
      (weights (build-list items-numb (lambda (x) (+ 1 (random WEIGHT_MAX)))))
      (k (+ 1 (random 1)))
    )

    ; cost and weight are independent
    (define (gen-uncorrelated) 
      (list
        "uncorrelated"
        weights
        (build-list items-numb (lambda (x) (+ 1 (random COST_MAX))))
        W
      )
    )

    ; cost in const neighbourhood of weight
    (define (gen-weakly-correlated)
      (let ( (weights-shifted (map (lambda (x) (+ WEAKLY_CORR_NBH x)) weights)) )
        (list
          "weakly-correlated"
          weights-shifted
          (map (lambda (x) (+ (- x WEAKLY_CORR_NBH) (random (* 2 (+ 1 WEAKLY_CORR_NBH))))) weights-shifted)
          (+ W WEAKLY_CORR_NBH)
        )
      )
    )

    ; cost is linear fuction of weight + const
    (define (gen-strongly-correlated)
      (list
        "strongly correlated"
        weights
        (map (lambda (x) (+ (* k x) STRONG_CORRELATING_CONST)) weights)
        W
      )
    )

    ; cost is linearly depends on weight
    (define (gen-subset-sum)
      (list
        "subset-sum"
        weights
        (map (lambda (x) (* k x)) weights)
        W
      )
    )

    ; generates big test
    (define (gen-big)
      ; answer part
      (define (gen-big-answer answer-size min-cost)
        (cons
          (build-list answer-size (lambda (x) (+ 1 (random WEIGHT_MAX))))
          (build-list answer-size (lambda (x) (+ (+ 2 min-cost) (random COST_MAX))))
        )
      )
      
      ; useless part
      (define (gen-useless size min-cost max-weight)
        (cons
          (build-list size (lambda (x) (+ (+ 1 max-weight) (random WEIGHT_MAX))))
          (build-list size (lambda (x) (+ 1 (random min-cost))))
        )
      )

      (let*
        (
          (answer-size 10)
          (useless-size 30)
          (min-cost 15)
          (answer-wc (gen-big-answer answer-size min-cost))
          (answer-weights (car answer-wc))
          (answer-costs (cdr answer-wc))
          (max-weight (max answer-weights (lambda (x) x)))
          (B (lst-sum answer-weights))
          (ready-answer
            (list
              B
              (lst-sum answer-costs)
              (build-list (length answer-weights) (lambda (x) (+ x 1)))
            )
          )
          (useless-wc (gen-useless useless-size min-cost max-weight))
          (useless-weights (car useless-wc))
          (useless-costs (cdr useless-wc))
          (weights (append answer-weights useless-weights))
          (costs (append answer-costs useless-costs))
          (task
            (list
              weights
              costs
              B
            )
          )
        )
        (cons task ready-answer)
      )
    )

    ; generates (test . solution) pair, getting solution with simple-solve
    (define (test-solution-ss test)
      (let*
        (
          (weights (list-ref test 1))
          (costs (list-ref test 2))
          (B (list-ref test 3))
          (solution (simple-solve weights costs B))  
        )
        (cons test solution)
      )  
    )

    (define (test-solution-big)
      (let*
        (
          (gbt (gen-big))
          (test-with-answer
            (cons
              (cons "big test" (car gbt))
              (cdr gbt)
            )
          )
          (task (car test-with-answer))
          (solution (cdr test-with-answer))
        )
        (cons task solution)
      )
    )

    (let*
      (
        (selector (random))
        (false-selector (random))
        ; (test . solution) pair
        (test-solution
          (cond
            ((< selector 0.1) (test-solution-ss (gen-uncorrelated)))
            ((< selector 0.1) (test-solution-ss (gen-weakly-correlated)))
            ((< selector 0.1) (test-solution-ss (gen-strongly-correlated)))
            ((< selector 0.1) (test-solution-ss (gen-subset-sum)))
            (else  (test-solution-big))
          )
        )
        (test (car test-solution))
        (solution (cdr test-solution))
        (max-fit (list-ref solution 1))
      )
      (cond
        ;knapsack is too small for any item
        ((= max-fit 0) (cons (append-el test 1) (list #f)))
        (
          (<= false-selector FALSE_PROBABILITY)
          (cons (append-el test (* max-fit 2)) (list #f))
        )
        (else (cons (append-el test (quotient max-fit 2)) (cons #t solution)))
      )
    )
  )
)

;debug-level:
;-1 - print only tests (task and right answer)
;0 - only total results of testing
;1 - 0 + failed tests full info
;2 - all tests, all answers, total result
(define (start-testing n [debug-level 0])
  ; prints test info, if debug-level requires it
  (define (print-test success? test-type test-task test-answer given-answer)
    (print-only-test test-type test-task test-answer)
    (print-lst given-answer "given answer:")
    (if success?
      (displayln "Test has passed")
      (displayln "Test has failed")
    )
    (displayln "_________________________________")
  )
  
  (define (testing-cycle total-answers right-answers)
    (if (= total-answers n)
      (begin
        (newline)
        (displayln "Testing is finished")
        (printf "Tests have passed: ~a\n" right-answers)
        (printf "Tests have failed: ~a\n" (- total-answers right-answers))
        (printf "Total tests:  ~a\n" total-answers)
        (printf "Precision: ~a\n" (/ right-answers (* 1.0 total-answers)))
      )
      (let*
        (
          (test (gen-test))
          (test-type (caar test))
          (test-task (cdar test))
          (test-answer (cdr test))
          (weights (list-ref test-task 0))
          (costs (list-ref test-task 1))
          (B (list-ref test-task 2))
          (K (list-ref test-task 3))
          (given-answer (main-genetic weights costs B K))
          (t-or-f-right (car test-answer))
          (t-or-f-given (car given-answer))
        )

        ; compare right and given answers
        (cond
          (
            (and (equal? #f t-or-f-right) (equal? #f t-or-f-given))
            (begin
              (when (>= debug-level 2) (print-test #t test-type test-task test-answer given-answer))
              (testing-cycle (+ 1 total-answers) (+ 1 right-answers))
            )
          )
          (
            ; certainly wrong answer
            (xor t-or-f-right t-or-f-given)
            (begin
              (when (>= debug-level 1) (print-test #f test-type test-task test-answer given-answer))
              (testing-cycle (+ 1 total-answers) right-answers)
            )
          )
          (else
            (if (= (list-ref test-answer 2) (list-ref given-answer 2))
              (begin
                (when (>= debug-level 2) (print-test #t test-type test-task test-answer given-answer))
                (testing-cycle (+ 1 total-answers) (+ 1 right-answers))
              )
              (begin
                (when (>= debug-level 1) (print-test #f test-type test-task test-answer given-answer))
                (testing-cycle (+ 1 total-answers) right-answers)
              )
            )
          )
        )
      )
    )
  )

  (if (= debug-level -1)
    (print-tests n) 
    (testing-cycle 0 0)
  )
)

; just prints the test (task and right answer)
(define (print-only-test test-type test-task test-answer)
  (display "test type: ")
  (displayln test-type) 
  (print-lst test-task "task:")
  (newline)
  (print-lst test-answer "right answer:")
)

; print n tests
(define (print-tests n)
  (define (print-tests-cycle i)
    (if (= i n)
      (printf "Printing ~a tests done\n" i)
      (let*
        (
          (test (gen-test))
          (test-type (caar test))
          (test-task (cdar test))
          (test-answer (cdr test))
        )
        (begin
          (print-only-test test-type test-task test-answer)
          (displayln "_________________________________")
          (print-tests-cycle (+ 1 i))
        )
      )
    )
  )

  (print-tests-cycle 0)
)

;support functions

;maps bibary string to indexes, starting from 1
(define (binary-string-to-indexes bs)
  (filter
    (lambda (x) (not (= x -1)))
    (map
      (lambda (x)
        (if (car x)
          (cdr x)
          -1
        )
      )
      (zip bs (build-list (length bs) (lambda (x) (+ 1 x))))
    )
  )
)

(define (zip lst1 lst2)
  (map cons lst1 lst2)
)

;takes pos elements from lst
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

(define (percent-int number p)
  (inexact->exact (ceiling (* number (/ p 100.0))))
)

(define (xor b1 b2)
  (if (equal? b1 b2)
    #f
    #t
  )
)

; print list with title
(define (print-lst lst [title '()])
  (if (string? title)
    (displayln title)
    '()
  )
  (for ([i (in-list lst)])
    (displayln i))
)

(define (lst-sum lst)
  (foldl + 0 lst)
)

; f ~ <
; returns list of n max elements in lst, sorted in >
(define (max-els--lst-filtered lst f n)
  ; returns lst of n max elements in lst
  (define (max-els-cycle lst res)
    ; inserts el into < sorted list lst
    (define (insert el lst)
      (define (insert-cycle lst res)
        (cond
          ((null? lst) (reverse (cons el res)))
          ((f (car lst) el) (insert-cycle (cdr lst) (cons (car lst) res)))
          (else (append (reverse res) (cons el lst)))
        )
      )

      (let ( (inserted (insert-cycle lst '())) )
        (if (> (length inserted) n)
          (cdr inserted)
          inserted
        )
      )
    )

    (if (null? lst)
      (reverse res)
      (max-els-cycle (cdr lst) (insert (car lst) res))
    )
  )

  (max-els-cycle lst '())
)

(max-els--lst-filtered '(2 4 1 3 4) < 3)
;(main-genetic
;  '(36 37 5 85 99 84 83 19 13 55 92 99 71 44 85)
;  '(46 47 15 95 109 94 93 29 23 65 102 109 81 54 95)
;  670
;  395
;)
#|(main-dummy|#
  ;'(36 37 5 85 99 84 83 19 13 55 92 99 71 44 85)
  ;'(46 47 15 95 109 94 93 29 23 65 102 109 81 54 95)
  ;670
  ;395
#|)|#

;(main-genetic '(1 1 2 2) '(4 3 2 1) 3 4)
;(main-dummy '(1 1 2 2) '(4 3 2 1) 3 4)
;(main-genetic '(1 1 1 2) '(11 11 11 12) 3 20)
;(main-genetic '(1 1 1 2 72 22 77 10 55 35 72 41 30 62 82 21 46 82 59 2 34 53 36 53 72 22 77 10 55 35 72 41 30 62 82 21 46 82 59 2 34 53 36 53) '(11 11 11 12 72 22 77 10 55 35 72 41 30 62 82 21 46 82 59 2 34 53 36 53 72 22 77 10 55 35 72 41 30 62 82 21 46 82 59 2 34 53 36 53) 200 20)
;(let ( (v '(1 1 1 2 72 22 77 10 55 35 72 41 30 62 82 21 46 82 59 2 34 55 23 25)) )  
  ;(printf "n: ~a\n" (length v))
  ;(main-dummy v v 200 20)
;)
;(start-testing 1 2)
;(print-tests 15)

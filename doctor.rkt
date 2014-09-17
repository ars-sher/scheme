#lang scheme/base
(define (visit-doctor name)
  (define (doctor-driver-loop name memories)
    (define (reply user-response)
      (define (change-person phrase)
        (many-replace '((i you) (me you) (am are) (my your) (you i) (are am) (your my)) phrase)
      )

      (define (qualifier)
        (pick-random
          '(
            (you seem to think)
            (you feel that)
            (why do you believe)
            (why do you say)
            (please speak louder, it seemed to me you've said that)
            (you should pay no attention to the fact that)
            (is it truth that)
          )
        )
      )

      (define (hedge)
        (pick-random
          '(
            (please go on)
            (many people have the same sorts of feelings)
            (many of my patients have told me the same thing)
            (please continue)
            (by the way, what do you think about last United States House of Representatives elections?)
            (don't you mind listening some Portnoy? It helps)
            (would you like some soft French buns and tea?)
          )
        )
      )
      
      (let ( (strat (strategy)) (keyword-answer (get-keywords-answer user-response (keywords-answers))) )
        (if (not (null? keyword-answer))
          keyword-answer
          (cond
            ((= strat 0)
              (append (qualifier) (change-person user-response))
            )
            ((and (= strat 1) (> (length memories) 0))
              (append '(earlier you said that) (pick-random memories))
            )
            (else 
              (hedge)
            )
          )
        )
      )
    )
    
    (newline)
    (print '**)
    (let ( (user-response (read)) )
      (cond
        ((equal? user-response '(goodbye))
          (printf "Goodbye, ~a!\n" name)
          (printf "see you next week\n")
          (let ( (new-name (ask-patient-name)) )
            (if (equal? new-name 'enough!)
              (print '(Time to go!))
              (visit-doctor new-name)
            )
          )
        )
        (else
          (print (reply user-response))
          (doctor-driver-loop name (cons user-response memories))
        )
      )
    )
  )

  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name '())
)

(define (contains-word? lst word)
  (if (null? lst)
      #f
      (if (equal? (car lst) word)
        #t
        (contains-word? (cdr lst) word)
      )
  )
)

(define (search-in-map phrase map)
  (if (null? phrase)
    '()
    (let ( (word (car phrase)) )
      (if (contains-word? (car map) word)
        (many-replace  (list (list '__ word)) (pick-random (cadr map)))
        (search-in-map (cdr phrase) map)
      )
    )
  )
)

(define (get-keywords-answer phrase answers)
  (if (null? answers)
    '()
    (let ( (search-in-map-res (search-in-map phrase (car answers))) )
      (if (not (null? search-in-map-res))
        search-in-map-res
        (get-keywords-answer phrase (cdr answers))
      )
    )
  )
)

(define (keywords-answers)
  '(
    (
      (depressed suicide)
      (
        (when you feel depressed, go out for an ice cream)
        (depression is a disease that can be treated)
      )
    )
    (
      (mother father parents)
      (
        (tell me more about your __)
        (why do you feel that way about your __ ?)
      )
    )
    (
      (tired lazy)
      (
        (get back to work, you sluggard fool!)
        (you certainly should take a vacation)
      )
    )
    (
      (loneliness depression desolation bore)
      (
        (a career in politics will releive you from the __)
        (maybe cannabis can suppress the feeling of __)
        (to get rid of __, try to marry someone)
      )
    )
  )
)

;p(true) = n1 / n2
(define (prob n1 n2)
  (< (random n2) n1)
)

;random [0;1]
(define (rand)
  (/ (random 1001) 1000)
)

(define (check-predicates triples response)
  (let ( (right-triples (filter (lambda (x) ((car x) response)) triples)) )
    (map (lambda (x) (cdr x)) right-triples)
  )
)

(define (pick-func func-weight-pairs)
  (define (rand)
    (/ (random 1001) 1000)
  )
  
  (define (normalize-weights)
    (define (get-sum)
      (let ( (weights (map (lambda (x) (cadr x)) func-weight-pairs)) )
        (foldl + 0 weights)
      )
    )
    
    (let ( (sum (get-sum)))
      (map
        (lambda (x) (list (car x) (/ (cadr x) sum)))
        func-weight-pairs
      )
    )
  )
  
  2
)

(define (triples)
  (list
    (list
      (lambda (response) #t)
      (lambda (response) 2)
      2
    )
    (list
      (lambda (response) #f)
      (lambda (response) 3)
      4
    )
    (list
      (lambda (response) #t)
      (lambda (response) 3)
      4
    )
  )
)

(define (strategy)
  (random 3)
)

(define (ask-patient-name)
  (printf "NEXT!\n")
  (printf "Who are you?\n")
  (car (read))
)

(define (pick-random lst)
  (list-ref lst (random (length lst)))
)

(define (replace-word replacement-pairs word)
  (if (null? replacement-pairs)
    word
    (let ((pair (car replacement-pairs)))
      (if (equal? (car pair) word)
          (cadr pair)
          (replace-word (cdr replacement-pairs) word)
      )
    )
  )
)

(define (many-replace replacement-pairs lst)
  (cond
    ((null? lst) '())
    (else
      (cons
         (replace-word replacement-pairs (car lst))
         (many-replace replacement-pairs (cdr lst))
      )
    )
  )
)  
 



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
      
      (let ((strat (strategy))) 
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
 



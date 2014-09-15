#lang scheme/base
(define (visit-doctor name)
  (define (doctor-driver-loop name)
    (define (reply user-response)
      (define (change-person phrase)
        (many-replace '((i you) (me you) (am are) (my your) (are am) (you i) (your my)) phrase)
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
      
      (cond 
        ((fifty-fifty)
          (append (qualifier) (change-person user-response))
        )
        (else 
          (hedge)
        )
      )
    )

    (newline)
    (print '**)
    (let ((user-response (read)))
      (cond
        ((equal? user-response '(goodbye))
          (printf "Goodbye, ~a!\n" name)
          (print '(see you next week))
        )
        (else
          (print (reply user-response))
          (doctor-driver-loop name)
        )
      )
    )
  )

  (printf "Hello, ~a!\n" name)
  (print '(what seems to be the trouble?))
  (doctor-driver-loop name)
)

(define (fifty-fifty)
  (= (random 2) 
     
     0)
)

(define (replace pattern replacement lst)
  (cond
    ((null? lst) '())
    ((equal? (car lst) pattern)
      (cons
        replacement
        (replace pattern replacement (cdr lst))
      )
    )
    (else
      (cons
        (car lst)
        (replace pattern replacement (cdr lst)))
    )
  )
)

(define (pick-random lst)
  (list-ref lst (random (length lst)))
)



(define (many-replace-old replacement-pairs lst)
  (cond 
    ((null? replacement-pairs) lst)
    (else
      (let ((pat-rep (car replacement-pairs)))
        (replace
          (car pat-rep)
          (cadr pat-rep)
          (many-replace (cdr replacement-pairs) lst)
        )
      )
    )
  )
)

(define (replace-word replacement-pairs word)
  (if (null? replacement-pairs)
      word
      (if (equal? (car lst) pattern)
      )
  )
)

(define (many-replace replacement-pairs lst)
  (cond
    ((null? lst) '())
    (else
      (
       cons(
         (replace-word replacement-pairs (car lst))
         (many-replace replacement-pairs (cdr lst))
       )
      )
    )
  )
)  
 



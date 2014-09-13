#lang scheme/base

(define (factorial n)
  (define (iter product counter)
    (if (> counter n)
        product
        (iter (* counter product)
              (+ counter 1))))
  (iter 1 1))

;ex 1.11 recursive: f(n) = f(n-1) + 2f(n - 2) + 3f(n - 3), n >= 3; f(n) = n, else 
(define (f1.11rec n)
  (if (< n 3)
    n
    (+ 
      (f1.11rec (- n 1))
      (* 2 (f1.11rec (- n 2)))
      (* 3 (f1.11rec (- n 3)))
    )
  )
)

;ex 1.11 iterative
(define (f1.11it n)
  (define (f1.11_iter a b c count)
    (if (= count n)
        b
        (f1.11_iter (+ a (* 2 b) (* 3 c)) a b (+ count 1))
    )
  )
  (if (< n 3)
    n
    (f1.11_iter 11 (+ 2 (* 2 1) 0) 2 3)
  )
)


;Pascal's triangle
(define (pt row col)
  (cond
    ((> col row) -1)
    ((or (= row col) (= 0 col)) 1)
    (else (+ (pt (- row 1) (- col 1)) (pt (- row 1) col)))
  )
)
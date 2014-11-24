#lang scheme/base

; call from the terminal:
; !racket %:p

(require racket/class)
(require racket/draw)
(require racket/gui)
(require racket/math)
 
(define no-pen (new pen% [style 'transparent]))
(define no-brush (new brush% [style 'transparent]))
; (define blue-brush (new brush% [stipple (read-bitmap "water.png")]))
(define blue-brush (new brush% [color "blue"]))
(define yellow-brush (new brush% [color "yellow"]))
(define red-pen (new pen% [color "brown"] [width 2]))
 
(define (draw-result dc knapsack-width knapsack-height items-lst max-weight)
  ; number of different colors for items of different cost
  (define cost-categories 5)
  (define knapsack-x 445)
  (define knapsack-y 175)

  (define max-cost
    (apply
      max
      (map (lambda (x) (cdr x)) items-lst)
    )
  )
  (define min-cost
    (apply
      min
      (map (lambda (x) (cdr x)) items-lst)
    )
  )
  (define total-weight
    (lst-sum (map (lambda (x) (car x)) items-lst))
  )
  (define total-cost
    (lst-sum (map (lambda (x) (cdr x)) items-lst))
  )

  ; step between cost categories
  (define seg (/ (- max-cost min-cost) cost-categories))

  (define cost-categories-lst
    (list
      (cons (+ min-cost (* 1 seg)) (new brush% [color "gray"]))
      (cons (+ min-cost (* 2 seg)) (new brush% [color "blue"]))
      (cons (+ min-cost (* 3 seg)) (new brush% [color "green"]))
      (cons (+ min-cost (* 4 seg)) (new brush% [color "orange"]))
      (cons (+ min-cost (* 5 seg)) (new brush% [color "yellow"]))
    )
  )

  ; knapsack volume path
  (define knapsack-path
    (let ([p (new dc-path%)])
      ; (send p append left-lambda-path)
      (send p move-to 0 0)
      (send p line-to 0 knapsack-height)
      (send p line-to knapsack-width knapsack-height)
      (send p line-to knapsack-width 0)
      (send p move-to 0 0)
      p
    )
  )

  ; put an item (weight . cost) to the knapsack
  (define (put-item i)
    (let*
      (
        (item-height (quot-int knapsack-height (/ (car i) max-weight)))
      )
      (send dc set-brush (get-item-color (cdr i)))
      (send dc translate 0 (- item-height))
      (send dc draw-rectangle 0 0 knapsack-width item-height)
    )
  )

  ; determine color of item, depending on its cost
  (define (get-item-color cost)
    (cond
      ((< cost (car (list-ref cost-categories-lst 0))) (cdr (list-ref cost-categories-lst 0)))
      ((< cost (car (list-ref cost-categories-lst 1))) (cdr (list-ref cost-categories-lst 1)))
      ((< cost (car (list-ref cost-categories-lst 2))) (cdr (list-ref cost-categories-lst 2)))
      ((< cost (car (list-ref cost-categories-lst 3))) (cdr (list-ref cost-categories-lst 3)))
      ((<= cost (car (list-ref cost-categories-lst 4))) (cdr (list-ref cost-categories-lst 4)))
      (else (cdr (list-ref cost-categories-lst 4)))
    )
  )

  (define title
    (let 
      (
        (p (new dc-path%))
        (font (make-font #:size 24 #:family 'swiss #:weight 'bold))
        (str (string-append "Knapsack filled " (number->string (/ total-weight max-weight))))
      )
      (send p text-outline font str 0 0)
      p
    )
  )

  (define total-cost-path
    (let*
      (
        (p (new dc-path%))
        (font (make-font #:size 20 #:family 'swiss #:weight 'bold))
        (str (string-append "Total cost: " (number->string total-cost)))
      )
      (send p text-outline font str 0 0)
      p
    )
  )

  (define (draw-mapping brush start end)
    (let 
      (
        (font (make-font #:size 12 #:family 'swiss #:weight 'bold))
        (str (string-append "cost in [" (number->string start) ";" (number->string end) "]"))
      )
      (send dc set-brush brush)
      (send dc draw-rectangle 0 0 30 30)
      (send dc set-font font)
      (send dc draw-text str 40 2)
      (send dc translate 0 (- 50))
    )
  )

  (define (draw-mapping-cycle lst n)
    (if (= (length lst) 1)
      2
      (let*
        (
          (brush (cdadr lst))
          (end (caadr lst))
          (start (caar lst))
        )
        (draw-mapping brush start end)
        (draw-mapping-cycle (cdr lst) n)
      )
    )
  )

  (let
    (
      (items-lst-sorted (sort items-lst (lambda (x y) (> (car x) (car y)))))
      (no-transformation (send dc get-transformation))
    )
    (send dc set-smoothing 'smoothed)

    (send dc translate knapsack-x knapsack-y)
    ; draw knapsack volume
    (send dc set-pen "black" 2 'long-dash)
    (send dc set-brush "white" 'transparent)
    (send dc draw-path knapsack-path)

    ; put items
    (send dc set-pen "black" 1 'solid)
    (send dc translate 0 knapsack-height)
    (foreach items-lst-sorted put-item) 
    (send dc set-transformation no-transformation)

    ; (foreach items-lst-sorted displayln)
    ; (displayln cost-categories-lst) 

    ; draw title
    (send dc translate 180 50)
    (send dc set-brush "black" 'solid)
    (send dc draw-path title)
    (send dc set-transformation no-transformation)

    ; draw mapping
    (send dc translate 50 500)
    ; draw initual segment
    (draw-mapping (cdr (list-ref cost-categories-lst 0)) min-cost (car (list-ref cost-categories-lst 0)))
    ; draw other segments
    (draw-mapping-cycle cost-categories-lst 1)
    (send dc set-transformation no-transformation)

    ;draw total cost
    (send dc translate 50 200)
    (send dc set-brush "black" 'solid)
    (send dc draw-path total-cost-path)
    (send dc set-transformation no-transformation)
  )
)

(define (draw-face dc)
  (send dc set-smoothing 'aligned)
 
  (send dc set-pen no-pen)
  (send dc set-brush blue-brush)
  (send dc draw-ellipse 25 25 100 100)
 
  (send dc set-brush yellow-brush)
  (send dc draw-rectangle 50 50 10 10)
  (send dc draw-rectangle 90 50 10 10)
 
  (send dc set-brush no-brush)
  (send dc set-pen red-pen)
  (send dc draw-arc 37 37 75 75 (* 5/4 pi) (* 7/4 pi))
)

(define (quot-int number q)
  (inexact->exact (ceiling (* number q)))
)

(define (foreach lst f)
  (if (null? lst)
    2
    (begin
      (f (car lst))
      (foreach (cdr lst) f)
    )
  )
)
;_______________________
(define (zip lst1 lst2)
  (map cons lst1 lst2)
)

(define (lst-sum lst)
  (foldl + 0 lst)
)

(let*
  (
    (width 695)
    (heigth 637)
    (width-center (/ width 2))
    (heigth-center (/ heigth 2))

    (target (make-bitmap width heigth))
    (usls (send target load-file "knapsack.jpg"))
    (dc (new bitmap-dc% [bitmap target]))
    (no-transformation (send dc get-transformation))

    ; (knapsack-width (quot-int width 0.4))
    ; (knapsack-height (quot-int heigth 0.5))
    (knapsack-width 120)
    (knapsack-height 310)
    (max-weight 15)
    (items-lst (zip '(2 4 5 1 1 1) '(10 15 20 40 60 70)))
    (usls (draw-result dc knapsack-width knapsack-height items-lst max-weight))

    (usls (send dc set-smoothing 'smoothed))
    ; (usls (draw-face dc))
  )
  (send target save-file "box.png" 'png)
  ;(make-object image-snip% target)
)

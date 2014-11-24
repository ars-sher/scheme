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
      (send dc translate 0 (- 40))
    )
  )

  ; (define (draw-mapping-cycle lst n)
  ;   (if (= (length cost-categories-lst) n)
  ;     2
  ;     (begin
  ;       (draw-mapping
  ;         (cdr (list-ref lst n))
  ;         (car (list-ref lst (- n 1)))
  ;         (car (list-ref lst n))
  ;       )
  ;       (draw-mapping-cycle lst (+ n 1))
  ;     )
  ;   )
  ; )

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
    (send dc set-pen "red" 1 'solid)
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

(define (draw-racket-logo dc)
  (define left-lambda-path
    (let ([p (new dc-path%)])
      (send p move-to 153 44)
      (send p line-to 161.5 60)
      (send p curve-to 202.5 49 230 42 245 61)
      (send p curve-to 280.06 105.41 287.5 141 296.5 186)       
      (send p curve-to 301.12 209.08 299.11 223.38 293.96 244)
      (send p curve-to 281.34 294.54 259.18 331.61 233.5 375)
      (send p curve-to 198.21 434.63 164.68 505.6 125.5 564)
      (send p line-to 135 572)
      p
    )
  )
  
  (define left-logo-path
    (let ([p (new dc-path%)])
      (send p append left-lambda-path)
      (send p arc 0 0 630 630 (* 47/72 2 pi) (* 121/360 2 pi) #f)
      p
    )
  )
  
  (define bottom-lambda-path
    (let ([p (new dc-path%)])
      (send p move-to 135 572)
      (send p line-to 188.5 564)
      (send p curve-to 208.5 517 230.91 465.21 251 420)
      (send p curve-to 267 384 278.5 348 296.5 312)
      (send p curve-to 301.01 302.98 318 258 329 274)
      (send p curve-to 338.89 288.39 351 314 358 332)
      (send p curve-to 377.28 381.58 395.57 429.61 414 477)
      (send p curve-to 428 513 436.5 540 449.5 573)
      (send p line-to 465 580)
      (send p line-to 529 545)
      p
    )
  )
  
  (define bottom-logo-path
    (let ([p (new dc-path%)])
      (send p append bottom-lambda-path)
      (send p arc 0 0 630 630 (* 157/180 2 pi) (* 47/72 2 pi) #f)
      p
    )
  )

  (define right-lambda-path
    (let ([p (new dc-path%)])
      (send p move-to 153 44)
      (send p curve-to 192.21 30.69 233.21 14.23 275 20)
      (send p curve-to 328.6 27.4 350.23 103.08 364 151)
      (send p curve-to 378.75 202.32 400.5 244 418 294)
      (send p curve-to 446.56 375.6 494.5 456 530.5 537)
      (send p line-to 529 545)
      p
    )
  )  
  
  (define right-logo-path
    (let ([p (new dc-path%)])
      (send p append right-lambda-path)
      (send p arc 0 0 630 630 (* 157/180 2 pi) (* 121/360 2 pi) #t)
      p
    )
  )
  
  (define lambda-path
    (let ([p (new dc-path%)])
      (send p append left-lambda-path)
      (send p append bottom-lambda-path)
      (let ([t (new dc-path%)])
          (send t append right-lambda-path)
          (send t reverse)
          (send p append t)
      )
      (send p close)
      p
    )
  )
  
  (send dc set-pen "black" 0 'transparent)
  (send dc set-brush "white" 'solid)
  (send dc draw-path lambda-path)
 
  (send dc set-pen "black" 4 'solid)
  (send dc set-brush yellow-brush)
  (send dc draw-path left-logo-path)

  ;(send dc draw-path bottom-logo-path)
 
  (send dc set-brush blue-brush)
  ;(send dc draw-path right-logo-path)
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
    (usls (draw-face dc))

    ; (usls (send dc scale 0.5 0.5))
    ; (usls (draw-racket-logo dc))
  )
  (send target save-file "box.png" 'png)
  ;(make-object image-snip% target)
)

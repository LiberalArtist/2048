#lang racket/base

(require pict
         racket/draw
         racket/contract
         racket/math
         "color.rkt"
         "stx.rkt")

(provide (all-from-out "color.rkt")
         (all-from-out "stx.rkt")
         filled-rounded-square
         2048-text
         trim-descent
         (contract-out
          [guard-font-size
           guard-font-size/c]
          [scale/max-width
           (-> pict? (>/c 0) pict?)]))

(define/final-prop guard-font-size/c
  (-> real? (integer-in 1 1024)))

(define/contract (guard-font-size size)
  guard-font-size/c
  (cond
    [(size . < . 1)
     1]
    [(size . > . 1024)
     1024]
    [else
     (exact-floor size)]))

(define (filled-rounded-square size
                               color
                               #:radius [radius 10])
  (filled-rounded-rectangle size size radius
                            #:color color
                            #:draw-border? #f))

(define (2048-text str [size 12] #:color [color #f] #:bold? [bold? #t])
  (let* ([style '("Source Sans Pro" . swiss)]
         [style (if bold? (cons 'bold style) style)]
         [style (if color (cons color style) style)])
    (text str style (guard-font-size size))))

(define (trim-descent pict allow)
  (inset pict 0 0 0 (- (- (pict-descent pict)
                          allow))))

(define (scale/max-width pict w)
  (if ((pict-width pict) . <= . w)
      pict
      (scale-to-fit pict w (pict-height pict))))


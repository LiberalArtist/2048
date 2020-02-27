#lang racket/base

;; only used by "../write-icons.rkt"

(require pict
         racket/draw
         racket/class
         racket/contract)

(provide (contract-out
          [file-icon
           any/c
           #;(->i {}
                {#:width [w real?]
                 #:height [h (w); corner-w)
                             ;(if (unsupplied-arg? corner-w)
                             (and/c real? (>/c (/ w 3)))]
                 ;real?)]
                 #|#:corner-width
                 [corner-w (w)
                           (and/c real? (</c w))]
                 [corner-h (h)
                           (and/c real? (</c h))]|#
                 #:brush [brush (or/c #f (is-a?/c brush%))]}
                [_ pict?])]
          ))

(define ((make-draw-file-icon w h corner-w corner-h page-brush)
         dc x y)
  (define right-x
    (+ x w))
  (define bottom-y
    (+ y h))
  (define corner-left-x
    (- right-x corner-w))
  (define corner-bottom-y
    (+ y corner-h))
  (define corner-border-path
    (let ([corner-border-path (new dc-path%)])
      (send corner-border-path move-to right-x corner-bottom-y)
      (send corner-border-path line-to corner-left-x corner-bottom-y)
      (send corner-border-path line-to corner-left-x y)
      corner-border-path))
  (define page-path
    (let ([page-path (new dc-path%)])
      (send page-path append corner-border-path)
      (send page-path line-to x y)
      (send page-path line-to x bottom-y)
      (send page-path line-to right-x bottom-y)
      (send page-path line-to right-x corner-bottom-y)
      (send page-path close)
      page-path))
  (define corner-path
    (let ([corner-path (new dc-path%)])
      (send corner-path append corner-border-path)
      (send corner-path line-to right-x corner-bottom-y)
      (send corner-path close)
      corner-path))
  (define old-brush
    (send dc get-brush))
  (when page-brush
    (send dc set-brush page-brush))
  (send dc draw-path page-path)
  (send dc
        set-brush
        (new brush% [color (send (send dc get-pen) get-color)]))
  (send dc draw-path corner-path)
  (send dc set-brush old-brush))


(define (file-icon #:height [h 100]
                   #:width [w (* 19/22 h)] ;950/11]
                   #:corner-width [corner-w (/ w 3)]
                   #:corner-height [corner-h corner-w]
                   #:brush [page-brush #f])
  (dc (make-draw-file-icon w h corner-w corner-h page-brush)
      w
      h))

(module+ main
  (require (submod ".."))
  (linewidth 5 (file-icon #:brush (new brush% [color "white"]))))







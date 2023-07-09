#lang racket/base

(require racket/draw
         (only-in "../kernel.rkt"
                  tile-value?)
         "stx.rkt"
         racket/contract
         racket/class)

(module+ test
  (require rackunit))

(provide (contract-out
          [window-background-color
           (is-a?/c color%)]
          [border-color
           (is-a?/c color%)]
          [black
           (is-a?/c color%)]
          [tile-value->text-color
           (-> tile-value? (is-a?/c color%))]
          [tile-value->background-color
           (-> tile-value? (is-a?/c color%))]))

;; TODO: make the color scheme configurable

(define window-background-color
  (make-color 250 248 239))

(define border-color
  (make-color 187 173 160))

(define (border-color-offset Δ)
  (make-color (+ Δ (send border-color red))
              (+ Δ (send border-color green))
              (+ Δ (send border-color blue))
              (send border-color alpha)))

(define black
  (make-color 0 0 0))

(define (tile-value->text-color v)
  black)




(define (tile-value->background-color v)
  (tile-value-case
   #:of v
   ;; Earth tones
   [#f (border-color-offset 20)]
   [1 (border-color-offset 60)]
   [2 (make-color 237 224 200)]
   [3 (make-color 255 181 104)]
   [4 (make-color 204 114 71)]
   [5 (make-color 219 102 28)]
   [6 (make-color 217 50 0)]
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Yellow
   [7 (make-color #xFF #xF9 #xC4)] ;; Material 100
   [8 (make-color #xFF #xF1 #x76)] ;; Material 300
   [9 (make-color #xFB #xC0 #x2D)] ;; Material 700 
   [10 (make-color #xFF #xEB #x3B)] ;; Material 500 ;; ? or try half-way from 512 to 2048?
   [11 (make-color 255 255 0)] ;; Material A200 (vihart)
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Blues
   [else (make-color 33 186 255)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Old dark color schemes:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; v0
#|
(define window-background-color
  black)
(define border-color
  (make-color #x4E #x34 #x2E)) ;; Material Brown 800
(define (tile-value->text-color v)
  (tile-value-case
   #:of v
   [(5 6)
    ;(make-color #xEE #xEE #xEE)] ;; Material Grey 200
    (make-color #xBD #xBD #xBD)] ;; Material Grey 400
   [else
    black]))
(define (tile-value->background-color v)
  (tile-value-case
   #:of v
   [#f (make-color #x5D #x40 #x37)]
   ;; Material Grey
   [1 (make-color #xEE #xEE #xEE)] ;; Material Grey 200
   [2 (make-color #xE0 #xE0 #xE0)] ;; Material Grey 300
   [3 (make-color #xBD #xBD #xBD)] ;; Material Grey 400
   [4 (make-color #x9E #x9E #x9E)] ;; Material Grey 500
   [5 (make-color #x75 #x75 #x75)] ;; Material Grey 600
   [6 (make-color #x61 #x61 #x61)] ;; Material Grey 700
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Cyan
   [7 (make-color #x00 #x60 #x64)] ;; Material Cyan 900
   [8 (make-color #x00 #x83 #x8F)] ;; Material Cyan 800
   [9 ;(make-color #x00 #xAC #xC1)] ;; Material Cyan 600
    (make-color #x00 #x97 #xA7)] ;; Material Cyan 700
   [10 (make-color #x00 #xBC #xD4)] ;Material Cyan 500
   [11 (make-color #x00 #xE5 #xFF)] ;; Material Cyan A400
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Green
   [12 (make-color #x1B #x5E #x20)] ;; 900
   [else (make-color #x38 #x8E #x3C)])) ;; 700
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; v1 - like v0 except:
#|
(define (value->text-color v)
  (tile-value-case
   #:of v
   [(1 2) (make-color #x75 #x75 #x75)] ;; Material Grey 600
   [else black]))
(define border-color
  (make-color #x3E #x27 #x23)) ;; Material Brown 900
(define (tile-value->background-color v)
  ;; 32 and 64 are still too bright
  (tile-value-case
   #:of v
   [#f (make-color #x4E #x34 #x2E)] ;; Material Brown 800
   [2 (make-color #x42 #x42 #x42)] ;; Material Grey 800
   [1 (make-color #x21 #x21 #x21)] ;; Material Grey 900
   [6 (make-color #xBD #xBD #xBD)] ;; Material Grey 400
   [5 (make-color #x9E #x9E #x9E)] ;; Material Grey 500
   [4 (make-color #x75 #x75 #x75)] ;; Material Grey 600
   [3 (make-color #x61 #x61 #x61)] ;; Material Grey 700
   [else
    super]))
|#
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; v2 - like v1 except:
#|
(define (tile-value->background-color v)
  ;; 32 and 64 are still too bright
  (tile-value-case
   #:of v
   [#f (make-color #x4E #x34 #x2E)] ;; Material Brown 800
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Grey
   [1 (make-color #x21 #x21 #x21)] ;; Material Grey 900
   [2 (make-color #x42 #x42 #x42)] ;; Material Grey 800
   [3 (make-color #x61 #x61 #x61)] ;; Material Grey 700
   [4 (make-color #x75 #x75 #x75)] ;; Material Grey 600
   ;[5 (make-color #x9E #x9E #x9E)] ;; Material Grey 500
   ;[6 (make-color #xBD #xBD #xBD)] ;; Material Grey 400
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Green
   [5 (make-color #x1B #x5E #x20)] ;; 900
   [6 (make-color #x38 #x8E #x3C)] ;; 700
   [7 (make-color #x4C #xAF #x50)] ;; 500
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Cyan
   [8 (make-color #x00 #x60 #x64)] ;; Material Cyan 900
   [9 (make-color #x00 #x83 #x8F)] ;; Material Cyan 800
   [10 (make-color #x00 #xAC #xC1)] ;; Material Cyan 600
   ;(make-color #x00 #x97 #xA7)] ;; Material Cyan 700
   ;[10 (make-color #x00 #xBC #xD4)] ;; Material Cyan 500
   [11 (make-color #x00 #xE5 #xFF)] ;; Material Cyan A400
   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
   ;; Material Deep Orange
   [12 (make-color #xBF #x36 #x0C)] ;; 900
   [else (make-color #xDD #x2C #x00)])) ;; A700
|#

#lang info

(define collection "2048")
(define pkg-name "2048")
(define pkg-desc
  "The game 2048")
(define version "0.1")
(define pkg-authors '(philip))

(define gracket-launcher-libraries
  '("main/gui/2048.rkt"))
(define gracket-launcher-names
  '("2048"))

(define scribblings
  '(("scribblings/2048.scrbl" ())))

(define deps
  '(["base" #:version "7.6"]
    "draw-lib"
    "gui-lib"
    "icns"
    "pict-lib"
    "string-constants-lib"
    "typed-racket-lib"
    "typed-racket-more"))

(define build-deps
  '("racket-doc"
    "scribble-lib"
    "rackunit-lib"
    "rackunit-typed"))

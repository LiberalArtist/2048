#lang racket/gui

(module test '#%kernel)

(define f
  (new frame%
       [label "Demo"]))

(define row
  (new horizontal-panel%
       [parent f]))

(define auto
  (new list-box%
       [parent row]
       [label "Automatic"]
       [choices  '("1" "2" "3")]
       [style '(vertical-label single)]))
(define custom
  (new list-box%
       [parent row]
       [label "Custom"]
       [choices  '("a" "b" "c")]
       [style '(vertical-label single)]))

(send f show #t)

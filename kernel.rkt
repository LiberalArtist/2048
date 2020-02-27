#lang typed/racket/base

(require "kernel/game.rkt"
         (only-in "kernel/board.rkt"
                  Board
                  Row
                  Tile-Value
                  Direction
                  board?
                  tile-value?))

(provide (all-from-out "kernel/game.rkt")
         (all-from-out "kernel/board.rkt"))

(module contract racket/base
  (require racket/contract)
  (provide direction/c)
  (define direction/c
    (or/c 'left 'right 'up 'down)))

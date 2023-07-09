#lang typed/racket/base

(provide State-Tree
         state-tree?
         state-tree-undo-stack
         state-tree-score
         state-tree-board
         state-tree-game-over?
         state-tree-move
         new-state-tree
         saved->state-tree
         state-tree->saved-form)

(require "board.rkt"
         "saved.rkt"
         racket/promise
         typed/racket/random)

(module+ test
  (require typed/rackunit))

(define-type State-Tree state-tree)

(struct state-tree
  ([immediates : s-t-immediates]
   [lazy-branches : (U state-tree-branches
                       (Promise state-tree-branches))]))

(struct s-t-immediates
  ([undo-stack : (Listof Direction)]
   [score : Natural]
   [board : Board]))

(struct state-tree-branches
  ([left : Branch-Value]
   [right : Branch-Value]
   [up : Branch-Value]
   [down : Branch-Value]))

(define-type Branch-Value
  (U 'same (Pairof Natural state-tree)))

(: state-tree-undo-stack (-> state-tree (Listof Direction)))
(define (state-tree-undo-stack this)
  (s-t-immediates-undo-stack (state-tree-immediates this)))
(: state-tree-score (-> state-tree Natural))
(define (state-tree-score this)
  (s-t-immediates-score (state-tree-immediates this)))
(: state-tree-board (-> state-tree Board))
(define (state-tree-board this)
  (s-t-immediates-board (state-tree-immediates this)))

(: state-tree-game-over? (-> state-tree Boolean))
(define (state-tree-game-over? tree)
  (define branches (state-tree-force-branches tree))
  (and (eq? 'same (state-tree-branches-left branches))
       (eq? 'same (state-tree-branches-right branches))
       (eq? 'same (state-tree-branches-up branches))
       (eq? 'same (state-tree-branches-down branches))))

(: state-tree-move (-> state-tree Direction (U #f state-tree)))
(define (state-tree-move tree dir)
  (define branches (state-tree-force-branches tree))
  (define v
    (case dir
      [(left) (state-tree-branches-left branches)]
      [(right) (state-tree-branches-right branches)]
      [(up) (state-tree-branches-up branches)]
      [else (state-tree-branches-down branches)]))
  (and (pair? v) (cdr v)))

(: state-tree-force-branches (-> state-tree state-tree-branches))
(define (state-tree-force-branches this)
  (define pr (state-tree-lazy-branches this))
  (if (state-tree-branches? pr)
      pr
      (force pr)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(: new-state-tree (-> state-tree))
(define (new-state-tree)
  (define immediates
    (s-t-immediates null
                    0
                    (board-spawn-tile
                     (board-spawn-tile
                      #(#(#f #f #f #f)
                        #(#f #f #f #f)
                        #(#f #f #f #f)
                        #(#f #f #f #f))))))
  (state-tree immediates (make-lazy-branches immediates)))

(: saved->state-tree (-> Saved-State-Tree state-tree))
(define (saved->state-tree saved)
  (let vivify ([undo-stack : (Listof Direction) null]
               [score : Natural 0]
               [saved saved])
    (define board (car saved))
    (define more (cdr saved))
    (define immediates
      (s-t-immediates undo-stack score board))
    (state-tree
     immediates
     (cond
       [(list? more)
        (let*-values
            ([{left-br more}
              (values (cons 'left (car more)) (cdr more))]
             [{right-br more}
              (values (cons 'right (car more)) (cdr more))]
             [{up-br more}
              (values (cons 'up (car more)) (cdr more))]
             [{down-br}
              (cons 'down (car more))])
          (define (convert-branch-pair [pr : (Pairof Direction Saved-Branch-Value)])
            (define dir (car pr))
            (define v (cdr pr))
            (if (pair? v)
                (let ([Δscore (car v)])
                  (cons Δscore (vivify (cons dir undo-stack)
                                       (+ Δscore score)
                                       (cdr v))))
                v))
          (state-tree-branches
           (convert-branch-pair left-br)
           (convert-branch-pair right-br)
           (convert-branch-pair up-br)
           (convert-branch-pair down-br)))]
       [else
        (make-lazy-branches immediates)]))))

(: make-lazy-branches (-> s-t-immediates (Promise state-tree-branches)))
(define (make-lazy-branches immediates)
  (delay
    (define board (s-t-immediates-board immediates))
    (define score (s-t-immediates-score immediates))
    (define undo-stack (s-t-immediates-undo-stack immediates))
    (define (branch [dir : Direction]) : Branch-Value
      (define-values [moved score+]
        (board-move board dir))
      (if (equal? board moved)
          'same
          (cons score+
                (let ([immediates
                       (s-t-immediates (cons dir undo-stack)
                                       (+ score score+)
                                       (board-spawn-tile moved))])
                  (state-tree immediates
                              (make-lazy-branches immediates))))))
    (state-tree-branches
     (branch 'left)
     (branch 'right)
     (branch 'up)
     (branch 'down))))

(: state-tree->saved-form (-> state-tree Saved-State-Tree))
(define (state-tree->saved-form this)
  (define promise* (state-tree-lazy-branches this))
  (cons
   (s-t-immediates-board (state-tree-immediates this))
   (cond
     [(state-tree-branches? promise*)
      (branches->saved promise*)]
     [(promise-forced? promise*)
      (branches->saved (force promise*))]
     [else
      'lazy])))
  
(: branches->saved (-> state-tree-branches Saved-State-Branches))
(define (branches->saved this)
  (define (convert1 [this : Branch-Value]) : Saved-Branch-Value
    (if (pair? this)
        (cons (car this) (state-tree->saved-form (cdr this)))
        this))
  (list (convert1 (state-tree-branches-left this))
        (convert1 (state-tree-branches-right this))
        (convert1 (state-tree-branches-up this))
        (convert1 (state-tree-branches-down this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; These are here rather than in "board.rkt" because,
;; in the future, it might be nice to support:
;;   - configurable chance of 2 vs 4
;;   - deterministic randomness that could be saved with the game

(: board-spawn-tile (-> Board Board))
(define (board-spawn-tile old)
  (define blanks (board-find-blanks old))
  (if (null? blanks)
      old
      (let ([spot (random-ref blanks)])
        (board-insert old (car spot) (cdr spot) (std-get-2-or-4)))))

(define (std-get-2-or-4)
  (if (< (random) 1/10)
      2
      1))

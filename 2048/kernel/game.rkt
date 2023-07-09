#lang typed/racket/base

(provide 2048-Game
         2048-game?
         ;; ops
         game->board
         game-winning?
         game-over?
         game-score
         game-move
         game-undo
         game-redo
         ;; bookmarks
         game-bookmark-exists?
         game-bookmarks-changed?
         game-all-bookmarks
         game-set-auto-bookmark
         game-set-custom-bookmark
         game-goto-bookmark
         ;; new / read / write
         new-2048-game
         read*-2048-game
         read-2048-game
         fasl->2048-game
         write-2048-game
         2048-game->fasl)

(require "board.rkt"
         "saved.rkt"
         "tree.rkt")

(module+ test
  (require typed/rackunit))

(define-type 2048-Game 2048-game)

(struct 2048-game
  ([persistent-state : persistent-state]
   [now : State-Tree]
   [redo-stack : (Listof Direction)]
   ;; support undo of goto bookmark?
   [bookmarks : (Immutable-HashTable Bookmark-Key bookmark-value)]))

(struct bookmark-value
  ([tree : State-Tree]
   [redo-stack : (Listof Direction)]))

(struct persistent-state
  ([root-tree : State-Tree]
   [stack-self-cache : (Mutable-HashTable (Listof Direction) State-Tree)]
   [parent-cache : (Mutable-HashTable State-Tree State-Tree)]))

(: make-persistent-state (-> State-Tree persistent-state))
(define (make-persistent-state root-tree)
  (persistent-state root-tree (make-hash) (make-hasheq)))

(: new-2048-game (-> 2048-game))
(define (new-2048-game)
  (define root (new-state-tree))
  (2048-game (make-persistent-state root)
             root
             null
             #hasheqv()))

(: read*-2048-game (-> Input-Port 2048-game))
(define (read*-2048-game in)
  (2048-game-from-saved (read*-saved-2048-game in)))
(: read-2048-game (-> Input-Port 2048-game))
(define (read-2048-game in)
  (2048-game-from-saved (read-saved-2048-game in)))
(: fasl->2048-game (-> (U Bytes Input-Port) 2048-game))
(define (fasl->2048-game in)
  (2048-game-from-saved (fasl->saved-2048-game in)))
(: write-2048-game (-> 2048-game Output-Port Void))
(define (write-2048-game game out)
  (write-saved-2048-game (2048-game->saved-form game) out))
(: 2048-game->fasl (-> 2048-game Output-Port Void))
(define (2048-game->fasl game out)
  (saved-2048-game->fasl (2048-game->saved-form game) out)
  (void))



;                     
;                     
;                     
;                     
;                     
;                     
;    ;;;   ; ;;    ;; 
;   ;   ;  ;;  ; ;;  ;
;   ;   ;  ;;  ;  ;   
;  ;;   ;; ;;  ;;  ;; 
;   ;   ;  ;;  ;     ;
;   ;   ;  ;;  ; ;   ;
;    ;;;   ;;;;   ;;; 
;          ;;         
;          ;;         
;          ;;         
;                     


(: game->board (-> 2048-game Board))
(define (game->board game)
  (state-tree-board (2048-game-now game)))

(: game-winning? (-> 2048-game Boolean))
(define (game-winning? game)
  (board-winning? (game->board game)))

(: game-over? (-> 2048-game Boolean))
(define (game-over? game)
  (state-tree-game-over? (2048-game-now game)))

(: game-score (-> 2048-game Natural))
(define (game-score game)
  (state-tree-score (2048-game-now game)))

(: game-move (-> 2048-game Direction (U #f 2048-game)))
(define (game-move game dir)
  (define tree (state-tree-move (2048-game-now game) dir))
  (and tree
       (2048-game (2048-game-persistent-state game)
                  tree
                  null
                  (2048-game-bookmarks game))))

(: game-undo (-> 2048-game (U #f 2048-game)))
(define (game-undo game)
  (define now (2048-game-now game))
  (define stack (state-tree-undo-stack now))
  (cond
    [(null? stack)
     #f]
    [else
     (define persist (2048-game-persistent-state game))
     (define parent
       ;; non-false because stack is non-empty
       (assert (get-state-tree-parent persist now)))
     (2048-game persist
                parent
                (cons (car stack) (2048-game-redo-stack game))
                (2048-game-bookmarks game))]))

(: game-redo (-> 2048-game (U #f 2048-game)))
(define (game-redo game)
  (define stack (2048-game-redo-stack game))
  (and (pair? stack)
       (game-move game (car stack))))


;                                                               
;                                                               
;                                                               
;                                                               
;   ;;                   ;;                           ;;        
;   ;;                   ;;                           ;;        
;   ;;;;    ;;;    ;;;   ;;  ;;; ;; ;;;    ;;    ;; ; ;;  ;; ;; 
;   ;;  ;  ;   ;  ;   ;  ;;  ; ;; ;;  ;   ;  ;   ;;;  ;;  ;;;  ;
;   ;;  ;  ;   ;  ;   ;  ;; ;  ;; ;;  ;;     ;;  ;;   ;; ;  ;   
;   ;;  ;;;;   ;;;;   ;; ;;;;  ;; ;;  ;;   ;;;;  ;;   ;;;;   ;; 
;   ;;  ;  ;   ;  ;   ;  ;;  ; ;; ;;  ;;  ;  ;;  ;;   ;;  ;    ;
;   ;;  ;  ;   ;  ;   ;  ;;  ; ;; ;;  ;; ;;  ;;  ;;   ;;  ;;   ;
;   ; ;;    ;;;    ;;;   ;;   ;;; ;;  ;;  ;;; ;  ;;   ;;   ;;;; 
;                                                               
;                                                               
;                                                               
;                                                               

(: game-bookmark-exists? (-> 2048-game Any Boolean))
(define (game-bookmark-exists? game k)
  (hash-has-key? (2048-game-bookmarks game) k))

(: game-bookmarks-changed? (-> 2048-game 2048-game Boolean))
(define (game-bookmarks-changed? a b)
  (not (equal? (2048-game-bookmarks a)
               (2048-game-bookmarks b))))

(: game-all-bookmarks (-> 2048-game (Values (U #f Natural) (Listof Symbol))))
(define (game-all-bookmarks game)
  (for/fold ([auto : (U #f Natural) #f]
             [syms : (Listof Symbol) null]
             #:result (values auto syms))
            ([k (in-immutable-hash-keys (2048-game-bookmarks game))])
    (if (symbol? k)
        (values auto (cons k syms))
        (values (if auto (max auto k) k) syms))))

(: game-set-auto-bookmark (-> 2048-game (Values Natural 2048-game)))
(define (game-set-auto-bookmark game)
  (define key
    (for/fold ([used : (U #f Natural) #f]
               #:result (if used (add1 used) 0))
              ([k (in-immutable-hash-keys (2048-game-bookmarks game))]
               #:when (number? k))
      (if used (max used k) k)))
  (values key
          (do-set-bookmark game key)))

(: game-set-custom-bookmark (-> 2048-game Symbol 2048-game))
(define (game-set-custom-bookmark game k)
  (when (game-bookmark-exists? game k)
    (raise-arguments-error 'game-set-custom-bookmark
                           "bookmark already exists for the given game"
                           "bookmark" k
                           "game" game))
  (do-set-bookmark game k))

(: do-set-bookmark (-> 2048-game Bookmark-Key 2048-game))
(define (do-set-bookmark game k)
  (define now (2048-game-now game))
  (define redo-stack (2048-game-redo-stack game))
  (2048-game
   (2048-game-persistent-state game)
   now
   redo-stack
   (hash-set (2048-game-bookmarks game) k (bookmark-value now redo-stack))))

(: game-goto-bookmark (-> 2048-game Bookmark-Key 2048-game))
(define (game-goto-bookmark game k)
  (unless (game-bookmark-exists? game k)
    (raise-arguments-error 'game-goto-bookmark
                           "bookmark does not exist for the given game"
                           "bookmark" k
                           "game" game))
  (define bookmarks (2048-game-bookmarks game))
  (define dest (hash-ref bookmarks k))
  (2048-game
   (2048-game-persistent-state game)
   (bookmark-value-tree dest)
   (bookmark-value-redo-stack dest)
   bookmarks))
  

;                                         
;                                         
;                                         
;                                         
;   ;;           ;;                       
;   ;;           ;;                       
;   ;;;;;   ;;   ;; ; ;;    ;;   ;; ;  ;; 
;   ;;  ;  ;  ;  ;; ;;  ;  ;  ;  ;;; ;;  ;
;   ;;  ;; ;  ;  ;; ;;  ;  ;  ;  ;;   ;   
;   ;;  ;;;;;;;; ;; ;;  ;;;;;;;; ;;    ;; 
;   ;;  ;; ;     ;; ;;  ;  ;     ;;      ;
;   ;;  ;; ;     ;; ;;  ;  ;     ;;  ;   ;
;   ;;  ;;  ;;;   ; ;;;;    ;;;  ;;   ;;; 
;                   ;;                    
;                   ;;                    
;                   ;;                    
;                                         


(: get-state-tree-parent (-> persistent-state State-Tree (U #f State-Tree)))
(define (get-state-tree-parent persist this)
  (define undo-stack (state-tree-undo-stack  this))
  (define cache (persistent-state-parent-cache persist))
  (cond
    [(null? undo-stack)
     #f]
    [(hash-ref cache this #f)]
    [else
     (define parent (undo-stack->self-tree persist (cdr undo-stack)))
     (hash-set! cache this parent)
     parent]))

(: undo-stack->self-tree (-> persistent-state (Listof Direction) State-Tree))
(define (undo-stack->self-tree state stack)
  (define root (persistent-state-root-tree state))
  (define cache (persistent-state-stack-self-cache state))
  (define *cache-interval* : Positive-Index 10)
  (define (resolve [stack : (Listof Direction)]) : State-Tree
    (cond
      [(null? stack)
       root]
      [(= 0 (modulo (length stack) *cache-interval*))
       (cond
         [(hash-ref cache stack #f)]
         [else
          (define ret (resolve-step stack))
          (hash-set! cache stack ret)
          ret])]
      [else
       (resolve-step stack)]))
  (define (resolve-step [stack : (Listof Direction)]) : State-Tree
    (define parent (resolve (cdr stack)))
    (or (state-tree-move parent (car stack))
        parent))
  (resolve stack))

;                                                                    
;                                                                    
;                                                                    
;                                                                    
;                              ;;           ;;                       
;                              ;;           ;;                       
;    ;; ; ;;   ;    ;  ;;      ;;;;;   ;;   ;; ; ;;    ;;   ;; ;  ;; 
;  ;;  ; ;  ;   ;  ;  ;  ;     ;;  ;  ;  ;  ;; ;;  ;  ;  ;  ;;; ;;  ;
;   ;       ;;  ;  ;  ;  ;     ;;  ;; ;  ;  ;; ;;  ;  ;  ;  ;;   ;   
;    ;;   ;;;;  ;  ; ;;;;;;    ;;  ;;;;;;;; ;; ;;  ;;;;;;;; ;;    ;; 
;      ;;;  ;;   ; ;  ;        ;;  ;; ;     ;; ;;  ;  ;     ;;      ;
;  ;   ;;;  ;;   ;;   ;        ;;  ;; ;     ;; ;;  ;  ;     ;;  ;   ;
;   ;;;  ;;; ;   ;;    ;;;     ;;  ;;  ;;;   ; ;;;;    ;;;  ;;   ;;; 
;                                              ;;                    
;                                              ;;                    
;                                              ;;                    
;                                                                    


(: 2048-game-from-saved (-> saved-2048-game 2048-game))
(define (2048-game-from-saved saved)
  (define persist
    (make-persistent-state
     (saved->state-tree (saved-2048-game-tree saved))))
  (define saved-now (saved-2048-game-now saved))
  (define now (undo-stack->self-tree persist (undo/redo-undo-stack saved-now)))
  (2048-game persist
             now
             (undo/redo-redo-stack saved-now)
             (for/hasheqv
                 : (Immutable-HashTable Bookmark-Key bookmark-value)
               ([{k v} (in-immutable-hash
                        (saved-2048-game-bookmarks saved))])
               (define v*
                 (bookmark-value
                  (undo-stack->self-tree persist (undo/redo-undo-stack v))
                  (undo/redo-redo-stack v)))
               (values k v*))))

(: 2048-game->saved-form (-> 2048-game saved-2048-game))
(define (2048-game->saved-form game)
  (saved-2048-game
   (state-tree->saved-form
    (persistent-state-root-tree (2048-game-persistent-state game)))
   (undo/redo (state-tree-undo-stack (2048-game-now game))
              (2048-game-redo-stack game))
   (for/hasheqv
       : (Immutable-HashTable Bookmark-Key undo/redo)
     ([{k v} (in-immutable-hash (2048-game-bookmarks game))])
     (values k
             (undo/redo (state-tree-undo-stack (bookmark-value-tree v))
                        (bookmark-value-redo-stack v))))))







          








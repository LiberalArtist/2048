#lang typed/racket/base

(provide (struct-out saved-2048-game)
         Saved-State-Tree
         Saved-State-Branches
         Saved-Branch-Value
         Bookmark-Key
         (struct-out undo/redo)
         read*-saved-2048-game
         read-saved-2048-game
         write-saved-2048-game
         fasl->saved-2048-game
         saved-2048-game->fasl)

(require "board.rkt")

(define-type Saved-State-Tree
  (Pairof Board (U 'lazy Saved-State-Branches)))

(define-type Saved-Branch-Value
  (U 'same (Pairof Natural Saved-State-Tree)))

(define-type Saved-State-Branches
  (List Saved-Branch-Value ;; left
        Saved-Branch-Value ;; right
        Saved-Branch-Value ;; up
        Saved-Branch-Value)) ;; down

(define-type Bookmark-Key
  (U Natural Symbol))

(struct undo/redo
  ([undo-stack : (Listof Direction)]
   [redo-stack : (Listof Direction)])
  #:prefab)

(struct saved-2048-game
  ([tree : Saved-State-Tree]
   [now : undo/redo]
   [bookmarks : (Immutable-HashTable Bookmark-Key undo/redo)])
  #:prefab)

(module read-via-syntax racket/base
  ;; read reads vectors as mutable
  (provide read-via-syntax)
  (define (read-via-syntax in)
    (syntax->datum
     (read-syntax (object-name in) in))))
(require/typed
 'read-via-syntax
 [{read-via-syntax read-saved-2048-game}
  (-> Input-Port saved-2048-game)])

(require/typed
 racket/base
 [{write write-saved-2048-game}
  (-> saved-2048-game Output-Port Void)])

(require/typed
 racket/fasl
 [{s-exp->fasl saved-2048-game->fasl}
  (-> saved-2048-game Output-Port Any)]
 [{fasl->s-exp fasl->saved-2048-game}
  (-> (U Bytes Input-Port) saved-2048-game)])

(require/typed
 syntax/readerr
 [raise-read-eof-error
  (-> String
      Any
      (U #f Positive-Integer)
      (U #f Nonnegative-Integer)
      (U #f Positive-Integer)
      (U #f Nonnegative-Integer)
      Nothing)])

(define read-prefix #"#s(saved-2048-game")
(define fasl-prefix #"racket/fasl:")
(define read-prefix-length (bytes-length read-prefix))
(define fasl-prefix-length (bytes-length fasl-prefix))

(: read*-saved-2048-game (-> Input-Port saved-2048-game))
(define (read*-saved-2048-game in)
  ;; invariant: (< read-prefix-length fasl-prefix-length)
  (define the-prefix (peek-bytes read-prefix-length 0 in))
  (cond
    [(eof-object? the-prefix)
     (define-values [line col pos]
       (port-next-location in))
     (raise-read-eof-error "read*-saved-2048-game: unexpected eof"
                           (object-name in)
                           line
                           col
                           pos
                           read-prefix-length)]
    [(bytes=? read-prefix the-prefix) ;; no allocation
     (read-saved-2048-game in)]
    [(bytes=? fasl-prefix (subbytes the-prefix 0 fasl-prefix-length))
     (fasl->saved-2048-game in)]
    [else
     (raise-arguments-error 'read*-saved-2048-game
                            "input begins with an unknown prefix"
                            "prefix" the-prefix
                            "input" in
                            "expected" (list fasl-prefix read-prefix))]))


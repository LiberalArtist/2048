#lang racket/base

(require "kernel.rkt"
         (submod "kernel.rkt" contract)
         "multicast.rkt"
         racket/path
         racket/contract
         racket/file
         string-constants
         syntax/parse/define
         (for-syntax racket/base
                     syntax/parse/lib/function-header))

;; Naming conventions:
;;   - Functions that can mutate end with `!`
;;   - Accessor functions for values that may change
;;     include `get` to signify that they are not pure.
;;   - Field names are prefixed with `*` to prevent confusing
;;     them with exported accessor functions.

;; FIXME:
;;   - ?? accept path-string? or path? ??
;;   - ?? normalize path ??
;;   - ?? make state-save! explicit when reusing path ??

(provide state?
         (contract-out
          [make-state
           (->* [] [(or/c #f path-string?)] state?)]
          [state-get-game
           (-> state? 2048-game?)]
          [state-get-maybe-path
           (-> state? (or/c #f path-string?))]
          [state-get-has-unsaved-changes?
           (-> state? boolean?)]
          [state-subscribe/board*
           (-> state? evt?)]
          [state-subscribe/file
           (-> state? evt?)]
          [state-subscribe/bookmarks
           (-> state? evt?)]
          ;; Basic functions:
          [state-move!
           (-> state? direction/c any)]
          [state-undo!
           (-> state? any)]
          [state-redo!
           (-> state? any)]
          [state-get-can-undo?
           (-> state? boolean?)]
          [state-get-can-redo?
           (-> state? boolean?)]
          ;; Bookmark functions:
          [state-get-all-bookmarks
           (-> state? (values (or/c #f natural-number/c) (listof symbol?)))]
          [state-goto-bookmark!
           (-> state? (or/c natural-number/c symbol?) any)]
          [state-set-auto-bookmark!
           (-> state? natural-number/c)]
          [state-try-set-custom-bookmark!
           (-> state? symbol? boolean?)]
          ;; File functions:
          [state-save!
           (-> state? path-string? any)]
          [state-revert!
           (-> state? (or/c 'ok (cons/c exn:fail? path-string?)))]
          [struct (exn:fail:user:open-file-error exn:fail:user)
            ([message string?]
             [continuation-marks continuation-mark-set?]
             [path path-string?]
             [reason string?]
             [exn exn:fail?])]
          ))

(struct state
  (*lock
   [*maybe-path #:mutable]
   [*has-unsaved-changes? #:mutable]
   [*game #:mutable]
   *publisher/board* ;; board, score, winning, game-over
   *publisher/file
   *publisher/bookmarks))

(define (make-state [maybe-path #f])
  (state (make-semaphore 1)
         maybe-path
         (not maybe-path)
         (cond
           [maybe-path
            => open-game-file]
           [else
            (new-2048-game)])
         (make-multicast-publisher)
         (make-multicast-publisher)
         (make-multicast-publisher)))


(define-simple-macro (define/get-lock (name:id this:id . kw-formals:formals)
                       body:expr ...+)
  (define name
    (let* ([name (λ (this . kw-formals.params)
                   body ...)]
           [name (λ (this . kw-formals)
                   (parameterize-break #f
                     (call-with-semaphore/enable-break
                      (state-*lock this)
                      name
                      #f this . kw-formals.params)))])
      name)))


(define/get-lock (state-get-game this)
  (state-*game this))
(define/get-lock (state-get-maybe-path this)
  (state-*maybe-path this))
(define/get-lock (state-get-has-unsaved-changes? this)
  (state-*has-unsaved-changes? this))
;; Publishers are immutable, so subscribe* functions don't need lock.
(define (state-subscribe/board* this)
  (multicast-publisher-subscribe (state-*publisher/board* this)))
(define (state-subscribe/file this)
  (multicast-publisher-subscribe (state-*publisher/file this)))
(define (state-subscribe/bookmarks this)
  (multicast-publisher-subscribe (state-*publisher/bookmarks this)))

;; Imperative update helper:

(define (holding-lock:set-game! this game* #:for-revert? [for-revert? #f])
  (define old-changes? (state-*has-unsaved-changes? this))
  (define old-game (state-*game this))
  (when game*
    (set-state-*game! this game*)
    (set-state-*has-unsaved-changes?! this (not for-revert?))
    (when (game-bookmarks-changed? old-game game*)
      (multicast-publisher-publish! (state-*publisher/bookmarks this)))
    (when (or for-revert? (not old-changes?))
      (multicast-publisher-publish! (state-*publisher/file this)))
    (multicast-publisher-publish! (state-*publisher/board* this))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Basic functions:

(define/get-lock (state-move! this dir)
  (holding-lock:set-game! this (game-move (state-*game this) dir)))
(define/get-lock (state-undo! this)
  (holding-lock:set-game! this (game-undo (state-*game this))))
(define/get-lock (state-redo! this)
  (holding-lock:set-game! this (game-redo (state-*game this))))
(define/get-lock (state-get-can-undo? this)
  (and (game-undo (state-*game this)) #t))
(define/get-lock (state-get-can-redo? this)
  (and (game-redo (state-*game this)) #t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bookmark functions:

(define/get-lock (state-get-all-bookmarks this)
  (game-all-bookmarks (state-*game this)))
(define/get-lock (state-goto-bookmark! this k)
  (holding-lock:set-game! this (game-goto-bookmark (state-*game this) k)))
(define/get-lock (state-set-auto-bookmark! this)
  (define-values [key game*]
    (game-set-auto-bookmark (state-*game this)))
  (holding-lock:set-game! this game*)
  key)
(define/get-lock (state-try-set-custom-bookmark! this k)
  (define old-game (state-*game this))
  (cond
    [(game-bookmark-exists? old-game k)
     #f]
    [else
     (holding-lock:set-game! this (game-set-custom-bookmark old-game k))
     #t]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File functions:

(define/get-lock (state-save! this pth)
  (call-with-atomic-output-file pth
    (λ (out tmp) (write-2048-game (state-*game this) out)))
  (set-state-*has-unsaved-changes?! this #f)
  (set-state-*maybe-path! this pth)
  (multicast-publisher-publish! (state-*publisher/file this)))

(define/get-lock (state-revert! this)
  (define maybe-path (state-*maybe-path this))
  (when maybe-path
    (let/ec return
      (holding-lock:set-game!
       #:for-revert? #t
       (with-handlers ([exn:fail? (λ (e)
                                    (return (cons e maybe-path)))])
         (open-game-file maybe-path)))))
  'ok)

(struct exn:fail:user:open-file-error exn:fail:user (path reason exn)
  #:transparent)

(define (open-game-file pth)
  (define (complain reason exn)
    (raise (exn:fail:user:open-file-error
            (format "open-game-file: could not open file\n  path: ~e\n  reason: ~a"
                    pth
                    reason)
            (current-continuation-marks)
            pth
            reason
            exn)))
  (with-handlers
      ([exn:fail:filesystem?
        (λ (e)
          (complain (if (file-exists? pth)
                        (exn-message e)
                        (format (string-constant cannot-open-because-dne)
                                pth))
                    e))])
    (define cannot-open-because-invalid
      "Cannot open ~a because it is not a valid 2048 game file.")
    (call-with-input-file* pth
      (λ (in)
        (with-handlers
            ([exn:fail?
              (λ (e)
                (complain (format cannot-open-because-invalid
                                  pth)
                          e))])
          (read*-2048-game in))))))

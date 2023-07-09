#lang racket/base

(require racket/gui
         framework
         string-constants
         "../state.rkt" ;; for exn type
         pict
         racket/class)

(provide text:for-display%
         message-box/open-file-error
         find-frame
         reveal-or-create-frame
         user-oks-revert
         pict->screen-bitmap
         forbid-set-label-mixin
         frame:compound-label-mixin
         frame:compound-label:trim-and-set-mixin)

(define text:for-display%
  (class text%
    ;; Like text:hide-caret/selection%
    ;; from "gui-lib/mrlib/syntax-browser.rkt",
    ;; which also wants this behavior w/o being a
    ;; text:basic<%>, which the version from the framework requires.
    (super-new)
    (inherit get-start-position
             get-end-position
             set-position
             hide-caret
             is-locked?)
    (define/augment (after-set-position)
      (hide-caret (= (get-start-position) (get-end-position)))
      (inner (void) after-set-position))
    (define/override (lock lock?)
      (when lock?
        (unless (is-locked?)
          (set-position 0)))
      (super lock lock?))
    #|END text:for-display%|#))


(define (message-box/open-file-error e [parent #f])
  (message-box (string-constant error-loading)
               (if (exn:fail:user:open-file-error? e)
                   (exn:fail:user:open-file-error-reason e)
                   (string-append "An unknown error occurred.\n\n"
                                  (exn-message e)))
               parent
               '(ok caution))
  (void))


(define (user-oks-revert [parent #f])
  (eqv? 1 (message-box/custom
           (string-constant are-you-sure-revert-title)
           (string-constant are-you-sure-revert)
           (string-constant revert)
           (string-constant cancel)
           #f
           parent
           '(caution default=2)
           2)))

(define (find-frame %)
  (for/first ([f (in-list (send (group:get-the-frame-group) get-frames))]
              #:when (is-a? f %))
    f))

(define (reveal-or-create-frame %)
  (send (or (find-frame %)
            (new %))
        show
        #t))

(define (pict->screen-bitmap p)
  (pict->bitmap p #:make-bitmap make-screen-bitmap))

;; Label

(define forbid-set-label-mixin
  (mixin [top-level-window<%>] []
    (super-new)
    (define/override-final (set-label v)
      (raise-arguments-error (object-name this%)
                             "set-label method not allowed"
                             "given" v))))

(define frame:compound-label<%>
  (interface (top-level-window<%>)
    [get-entire-label (->m string?)]))

(define frame:compound-label:trim-and-set<%>
  (interface (frame:compound-label<%>)
    [trim-and-set-label (->m string? any)]))

(define-values [frame:compound-label-mixin
                frame:compound-label:trim-and-set-mixin]
  (let ([| - 2048| " - 2048"])
    (define frame:compound-label:base-mixin
      (mixin [top-level-window<%>] [frame:compound-label<%>]
        ;; Based on frame:editor-mixin to cooperate with the group
        (init [(:label label)])
        (define base-label :label)
        (super-new [label (get-entire-label)])
        (define/public-final (get-entire-label)
          (string-append-immutable base-label | - 2048|))
        (define/override-final (get-label)
          base-label)
        (define/override (set-label new-base)
          (set! base-label new-base)
          (super set-label (get-entire-label)))))
    (define (frame:compound-label-mixin %)
      (forbid-set-label-mixin
       (frame:compound-label:base-mixin %)))
    (define frame:compound-label:trim-and-set-mixin*
      (mixin [frame:compound-label<%>] [frame:compound-label:trim-and-set<%>]
        (init label)
        (super-new [label (trim-label label)])
        (inherit/super set-label)
        (define/private (trim-label str)
          (define suffix-len (string-length | - 2048|))
          (define part-max-len (- 200 suffix-len))
          (string->immutable-string
           (gui-utils:trim-string str part-max-len)))
        (define/public (trim-and-set-label str)
          (define clean (trim-label str))
          (queue-callback
           (λ () (super set-label clean))))))
    (define (frame:compound-label:trim-and-set-mixin %)
      (forbid-set-label-mixin
       (frame:compound-label:trim-and-set-mixin*
        (frame:compound-label:base-mixin %))))
    (values frame:compound-label-mixin
            frame:compound-label:trim-and-set-mixin)))

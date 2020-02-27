#lang racket/base

(require framework/preferences
         racket/contract
         racket/symbol)

(provide with-redirected-preferences
         (contract-out
          [install-redirected-preferences-parameters
           (-> any)]
          [call-with-redirected-prefences
           (-> (-> any) any)]))

(define redirected-preference-prefix
  "2048:redirected:")

;; TODO: It's ok to install the redirect multiple times
;; on top of itself, but it might be better to avoid doing so
;; (which is the common case, probably).

(define preferences-to-redirect
  '(plt:framework-pref:framework:recently-opened-files/pos
    plt:framework-pref:framework:recent-max-count
    plt:framework-pref:framework:recent-items-window-w
    plt:framework-pref:framework:recent-items-window-h
    plt:framework-pref:framework:recently-opened-sort-by))

(define redirect-assocs
  (for/list ([k (in-list preferences-to-redirect)])
    (cons k
          (string->symbol
           (string-append-immutable redirected-preference-prefix
                                    (symbol->immutable-string k))))))

(define (key->redirected k)
  (cond
    [(assq k redirect-assocs)
     => cdr]
    [else
     k]))

(define make-redirects
  (case-lambda
    [()
     (make-redirects (preferences:low-level-get-preference)
                     (preferences:low-level-put-preferences))]
    [(outer-get outer-put)
     (define 2048:get-preference
       (case-lambda
         [(k)
          (outer-get (key->redirected k))]
         [(k fail)
          (outer-get (key->redirected k) fail)]))
     (define (2048:put-preferences ks vs)
       (outer-put (map key->redirected ks) vs))
     (values 2048:get-preference 2048:put-preferences)]))

(define (install-redirected-preferences-parameters)
  (define-values [get put]
    (make-redirects))
  (preferences:low-level-get-preference get)
  (preferences:low-level-put-preferences put))

(define (call-with-redirected-prefences thunk)
  (define-values [get put]
    (make-redirects))
  (parameterize ([preferences:low-level-get-preference get]
                 [preferences:low-level-put-preferences put])
    (thunk)))

(define-syntax-rule (with-redirected-preferences body0 body ...)
  (call-with-redirected-prefences
   (λ () body0 body ...)))


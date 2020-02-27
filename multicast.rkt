#lang racket/base

(require racket/contract)

(provide multicast-publisher?
         (contract-out
          [make-multicast-publisher
           (-> multicast-publisher?)]
          [multicast-publisher-publish!
           (-> multicast-publisher? any)]
          [multicast-publisher-subscribe
           (-> multicast-publisher? evt?)]))

;; inspired by Alexis King's alexis-multicast package

;; If a semaphore is reachable only throuh weak references,
;; a thread blocked on it may be garbage collected.
;; Therefore, we use a weak hash table with the wrapper event
;;  as the key and the semaphore as the value.
;; This keeps a strong reference to the semaphore for as long
;; as the wrapper event is reachable.
;; This is not a key-in-value problem:
;; the value is reachable through the key, not vice versa.

(struct multicast-publisher (subscriptions))

(define (make-multicast-publisher)
  (multicast-publisher (make-weak-hasheq)))

(define (multicast-publisher-publish! this)
  (for ([sema (in-weak-hash-values (multicast-publisher-subscriptions this))])
    (semaphore-post sema)))

(define (multicast-publisher-subscribe this)
  (define sema (make-semaphore))
  (define evt (wrap-evt sema void))
  (hash-set! (multicast-publisher-subscriptions this) evt sema)
  evt)

#lang racket/base

(require racket/include
         (for-syntax racket/base
                     racket/port))

(provide agpl)

(define agpl
  (include/reader
   "COPYING"
   (λ (src in)
     (cond
       [(eof-object? (peek-byte in))
        eof]
       [else
        (port-count-lines! in)
        (define-values [line0 col0 pos0]
          (port-next-location in))
        (define str
          (port->string in))
        (define-values [line1 col1 pos1]
          (port-next-location in))
        (define-values [line col]
          (if (and (number? line0) (number? col0))
              (values line0 col0)
              (values #f #f)))
        (define srcloc
          (list src line col pos0 (and pos0 pos1 (- pos1 pos0))))
        (datum->syntax #f str srcloc)]))))


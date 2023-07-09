#lang racket/base

(require syntax/parse/define
         (for-syntax racket/base
                     syntax/name))

(provide tile-value-case
         tile-value-prepared-lambda)

(begin-for-syntax
  (define-syntax-class tile-value-literal
    (pattern (~or* #f _:exact-nonnegative-integer))))

(define-syntax (tile-value-case stx)
  (define-syntax-class lhs+
    #:attributes {[v 1]}
    (pattern v0:tile-value-literal
             #:with (v ...) #'(v0))
    (pattern (v:tile-value-literal ...+)))
  (syntax-parse stx
    #:track-literals
    #:literals {else}
    [(_ #:of val:expr
        [lhs:lhs+ rhs:expr] ...
        [else
         (~optional (~and dynamic-else? #:dynamic))
         else-rhs:expr])
     #:with (rhs-tmp ...) (map syntax-local-lift-expression (syntax->list #'(rhs ...)))
     #:with else-rhs-tmp (if (attribute dynamic-else?)
                             #'else-rhs
                             (syntax-local-lift-expression #'else-rhs))
     #'(case val
         [(lhs.v ...) rhs-tmp] ...
         [else else-rhs-tmp])]))

(define-simple-macro (tile-value-prepared-lambda
                         #:for [v:tile-value-literal ...]
                       body:expr)
  #:with name (or (syntax-local-infer-name this-syntax)
                  #'anonymous-prepared-tile-value-proc)
  #:with (prepared ...) (generate-temporaries (syntax->list #'(v ...)))
  (let* ([name body]
         [prepared (name v)]
         ...
         [name (λ (val)
                 (case val
                   [(v) prepared]
                   ...
                   [else (name val)]))])
    name))


  

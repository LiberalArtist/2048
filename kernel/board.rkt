#lang typed/racket/base

(provide Board
         Row
         Tile-Value
         Direction
         tile-value?
         board?
         board-move
         board-find-blanks
         board-insert
         board-winning?)

(require racket/match
         syntax/parse/define
         (for-syntax racket/base
                     racket/syntax))

(module+ test
  (require typed/rackunit))

;; 131072 is max tile
;; http://puzzling.stackexchange.com/a/49

(define-type Tile-Value
  (U #f Positive-Integer))

(define-type Row
  (Immutable-Vector Tile-Value Tile-Value Tile-Value Tile-Value))

(define-type Board
  (Immutable-Vector Row Row Row Row))

(define-type Direction
  (U 'left 'right 'up 'down))

(: tile-value? (-> Any Boolean : Tile-Value))
(define (tile-value? v)
  (or (not v) (exact-positive-integer? v)))

(define-predicate board? Board)

(: board-winning? (-> Board Boolean))
(define (board-winning? board)
  (for*/fold ([? : Boolean #f])
             ([row : Row (in-vector board)]
              [v : Tile-Value (in-vector row)])
    (define ? (and v (<= 11 v)))
    #:final ?
    ?))

(module+ test
  (check-true
   (board-winning?
    #(#(3 6 2 #f)
      #(11 9 5 #f)
      #(2 8 1 #f)
      #(1 #f #f #f)))))

(: index4? (-> Any Boolean : #:+ Index4))
(define (index4? v)
  (define two : 2 2)
  (define three : 3 3)
  (or (eqv? 0 v) (eqv? 1 v) (eqv? two v) (eqv? three v)))

(define-sequence-syntax in-index4
  (λ (stx) (raise-syntax-error #f "only allowed in for clauses" stx))
  (syntax-parser
    [[(idx) (_)]
     #'[(idx) (:do-in ([(idx) 0])
                      #t
                      ([idx idx])
                      (index4? idx)
                      ([(idx) idx])
                      #t
                      #t
                      [(add1 idx)])]]))
                      

(: board-find-blanks (-> Board (Listof (Pairof Index4 Index4))))
(define (board-find-blanks board)
  (for/fold ([acc : (Listof (Pairof Index4 Index4)) null])
            ([i (in-index4)]
             [row (in-vector board)]
             #:when #t
             [j (in-index4)]
             [val (in-vector row)]
             #:unless val)
    (cons (cons i j) acc)))

(: board-insert (-> Board Index4 Index4 Tile-Value Board))
(define (board-insert board i j v)
  (define (update [r : Row])
    (define (tile [j* : Index4])
      (if (= j j*) v (vector-ref r j*)))
    (vector-immutable (tile 0) (tile 1) (tile 2) (tile 3)))
  (define (row [i* : Index4]) : Row
    (define r (vector-ref board i*))
    (if (= i i*)
        (update r)
        r))
  (vector-immutable (row 0) (row 1) (row 2) (row 3)))


;                                                                              
;                                                                              
;                                                                              
;                                                                              
;                                                              ;;        ;;;;; 
;                                                              ;;       ;;  ;; 
;   ;; ;  ;;;  ;   ;   ;     ; ;; ;;;    ;;;  ;    ;  ;;       ;;  ;;  ;;;;;;;;
;   ;;;  ;   ; ;  ; ;  ;     ;; ;;  ;   ;   ;  ;  ;  ;  ;      ;; ;  ;  ;;  ;; 
;   ;;   ;   ;  ; ; ; ;      ;; ;;  ;;  ;   ;  ;  ;  ;  ;      ;; ;  ;  ;;  ;; 
;   ;;  ;;   ;; ; ; ; ;  ;;; ;; ;;  ;; ;;   ;; ;  ; ;;;;;; ;;; ;;;;;;;; ;;  ;; 
;   ;;   ;   ;  ; ; ; ;      ;; ;;  ;;  ;   ;   ; ;  ;         ;; ;     ;;  ;; 
;   ;;   ;   ;  ; ; ; ;      ;; ;;  ;;  ;   ;   ;;   ;         ;; ;     ;;   ; 
;   ;;    ;;;    ;   ;       ;; ;;  ;;   ;;;    ;;    ;;;       ;  ;;;  ;;   ;;
;                                                                              
;                                                                              
;                                                                              
;                                                                              


(define-type Row* (List Tile-Value Tile-Value Tile-Value Tile-Value))

(: row*-move-left (-> Row* (Values Row Natural)))
(define row*-move-left
  (let ()
    (: row*-move-left (-> Row* (Values Row Natural)))
    (define (row*-move-left r)
      (find-a r null 0))
    (define (finish [acc : (Listof Positive-Integer)]
                    [score : Natural])
      (values (match acc
                [(list d c b a)
                 (vector-immutable a b c d)]
                [(list c b a)
                 (vector-immutable a b c #f)]
                [(list b a)
                 (vector-immutable a b #f #f)]
                [(list a)
                 (vector-immutable a #f #f #f)]
                [_
                 #(#f #f #f #f)])
              score))
    (define (find-a [r : (Listof Tile-Value)]
                    [acc : (Listof Positive-Integer)]
                    [score : Natural])
      : (Values Row Natural)
      (cond
        [(null? r)
         (finish acc score)]
        [(not (car r))
         (find-a (cdr r) acc score)]
        [else
         (let using-a ([a : Positive-Integer (car r)]
                       [r (cdr r)]
                       [acc acc])
           (let find-b ([r r])
             (if (null? r)
                 (finish (cons a acc) score)
                 (let ([b (car r)]
                       [r (cdr r)])
                   (cond
                     [(not b)
                      (find-b r)]
                     [(= a b)
                      ;; score is incremented when two tiles combine
                      ;; by the value of the new tile
                      (define new (add1 a))
                      (find-a r (cons new acc) (+ score (expt 2 new)))]
                     [else
                      (using-a b r (cons a acc))])))))]))
    row*-move-left))

(module+ test
  (: row*-move-left* (-> Row* (Pairof Row Natural)))
  (define (row*-move-left* arg)
    (call-with-values
     (λ () (row*-move-left arg))
     cons))
  (check-equal? (row*-move-left* '(#f #f #f #f))
                '[#(#f #f #f #f). 0])
  (check-equal? (row*-move-left* '(1 2 3 4))
                '[#(1 2 3 4) . 0])
  (check-equal? (row*-move-left* '(#f 1 4 2))
                '[#(1 4 2 #f) . 0])
  (check-equal? (row*-move-left* '(1 #f 1 1))
                '[#(2 1 #f #f) . 4]))


;                                                                     
;                                                                     
;                                                                     
;                                                                     
;   ;;                            ;;                                  
;   ;;                            ;;                                  
;   ;;;;    ;;;    ;;    ;; ;  ;;;;;     ; ;; ;;;    ;;;  ;    ;  ;;  
;   ;;  ;  ;   ;  ;  ;   ;;;  ;   ;;     ;; ;;  ;   ;   ;  ;  ;  ;  ; 
;   ;;  ;  ;   ;     ;;  ;;   ;   ;;     ;; ;;  ;;  ;   ;  ;  ;  ;  ; 
;   ;;  ;;;;   ;;  ;;;;  ;;  ;;   ;; ;;; ;; ;;  ;; ;;   ;; ;  ; ;;;;;;
;   ;;  ;  ;   ;  ;  ;;  ;;   ;   ;;     ;; ;;  ;;  ;   ;   ; ;  ;    
;   ;;  ;  ;   ; ;;  ;;  ;;   ;   ;;     ;; ;;  ;;  ;   ;   ;;   ;    
;   ; ;;    ;;;   ;;; ;  ;;    ;;; ;     ;; ;;  ;;   ;;;    ;;    ;;; 
;                                                                     
;                                                                     
;                                                                     
;                                                                     


(: board-move (-> Board Direction (Values Board Natural)))
(define (board-move this dir)
  (case dir
    [(left)
     (board-move-left this)]
    [(right)
     (board-move-right this)]
    [(up)
     (board-move-up this)]
    [else
     (board-move-down this)]))


(module+ test
  (: board-move* (-> Board Direction (Pairof Board Natural)))
  (define (board-move* this dir)
    (call-with-values
     (λ () (board-move this dir))
     cons))
  (let ([board #(#(1 #f 1 1)
                 #(1 1 #f #f)
                 #(2 #f 1 3)
                 #(#f 1 3 2))])
    (check-equal? (board-move* board 'left)
                  (cons #(#(2 1 #f #f)
                          #(2 #f #f #f)
                          #(2 1 3 #f)
                          #(1 3 2 #f))
                        8))
    (check-equal? (board-move* board 'right)
                  (cons #(#(#f #f 1 2)
                          #(#f #f #f 2)
                          #(#f 2 1 3)
                          #(#f 1 3 2))
                        8))
    (check-equal? (board-move* board 'up)
                  (cons #(#(2 2 2 1)
                          #(2 #f 3 3)
                          #(#f #f #f 2)
                          #(#f #f #f #f))
                        12))
    (check-equal? (board-move* board 'down)
                  (cons #(#(#f #f #f #f)
                          #(#f #f #f 1)
                          #(2 #f 2 3)
                          #(2 2 3 2))
                        12))))




(define-type Index4 (U 0 1 2 3))

(: board-ref-ij (-> Board Index4 Index4 Tile-Value))
(define (board-ref-ij b i j)
  (vector-ref (vector-ref b i) j))

(define-for-syntax board-indices
  '(([0 0] [0 1] [0 2] [0 3])
    ([1 0] [1 1] [1 2] [1 3])
    ([2 0] [2 1] [2 2] [2 3])
    ([3 0] [3 1] [3 2] [3 3])))
(define-for-syntax (board-indices-map proc idxs)
  (map (λ (row) (map (λ (ij) (apply proc ij)) row))
       idxs))
(define-for-syntax (index4-reverse i)
  (case i
    [(0) 3]
    [(1) 2]
    [(2) 1]
    [else 0]))
(begin-for-syntax
  (define-syntax-class ij-spec
    #:attributes {[i 2] [j 2] [flat 1]}
    (pattern (([i:expr j:expr] ...+)
              ...+)
             #:with (flat ...) #'((~@ (~@ i j) ...) ...)
             #:fail-unless (let ([datum (syntax->datum this-syntax)])
                             (and (= 4 (length datum))
                                  (andmap (λ (r) (= 4 (length r)))
                                          datum)))
             "wrong dimensions"))
  (define (generate-ij-temporaries base)
    ((make-syntax-introducer)
     (datum->syntax
      #f
      (board-indices-map (λ (i j)
                           (define (fmt sym)
                             (format-id base "~a-~a-~a-~a" base sym i j))
                           (list (fmt 'i) (fmt 'j)))
                         board-indices)))))
(define-syntax-parser define-ij-proc
  #:track-literals
  [(_ (name:id [pos:id (~literal :) T:expr] ...
               (~seq #:ij ij:id) ...)
      (~seq (~literal :) ResultT:expr)
      (~seq #:with with-lhs:expr with-rhs:expr) ...
      body:expr ...+)
   #:with (proc-name:id) (generate-temporaries (list #'name))
   #:with (ij-temps:ij-spec ...) (map generate-ij-temporaries
                                      (syntax->list #'(ij ...)))
   #:with (ij-flat ...)
   (for/list ([base (in-list (syntax->list #'(ij ...)))])
     (format-id base "~a.flat" base))
   #`(begin
       (define (proc-name [pos : T] ...
                          (~@ [ij-temps.flat : Index4] ...)
                          ...)
         : ResultT
         (define-syntax-parser transformed-body
           #:disable-colon-notation
           #:track-literals
           [(_)
            (~@ #:with (~var ij ij-spec) #'ij-temps) ...
            (~@ #:with with-lhs with-rhs) ...
            #'(begin body ...)])
         (transformed-body))
       (define-syntax-parser name
         #:disable-colon-notation
         #:track-literals
         [(_ (~var pos expr) ...
             (~var ij ij-spec) ...)
          #'(proc-name pos ...
                       ((... ~@) ij-flat (... ...))
                       ...)]))])

(define-ij-proc (do-board-move-begin [board : Board]
                                     #:ij shifted)
  : (Values Board Natural)
  #:with ([tmp-row tmp-score] ...)
  #'([r0 s0] [r1 s1] [r2 s2] [r3 s3])
  (define-values [tmp-row tmp-score]
    (row*-move-left
     (list (board-ref-ij board shifted.i shifted.j) ...)))
  ...
  (values (vector-immutable tmp-row ...)
          (+ tmp-score ...)))

(define-ij-proc (do-board-move-with-unshift [board : Board]
                                            #:ij shifted
                                            #:ij unshifted)
  : (Values Board Natural)
  (define-values [tmp-board score]
    (do-board-move-begin board shifted))
  (values (vector-immutable
           (vector-immutable (board-ref-ij tmp-board unshifted.i unshifted.j)
                             ...)
           ...)
          score))

(define-type Board-Move-Direction-Proc
  (-> Board (Values Board Natural)))

(: board-move-left Board-Move-Direction-Proc)
(define (board-move-left board)
  (define-syntax (body stx)
    #`(do-board-move-begin board
                           #,(datum->syntax #'board-move-left board-indices)))
  (body))

(: board-move-right Board-Move-Direction-Proc)
(define (board-move-right board)
  (define-syntax (body stx)
    (define indices
      (datum->syntax
       #'board-move-right
       (board-indices-map (λ (i j)
                            (list i (index4-reverse j)))
                          board-indices)))
    #`(do-board-move-with-unshift board #,indices #,indices))
  (body))

(: board-move-up Board-Move-Direction-Proc)
(define (board-move-up board)
  (define-syntax (body stx)
    (define indices
      (datum->syntax
       #'board-move-up
       (board-indices-map (λ (i j)
                            (list j i))
                          board-indices)))
    #`(do-board-move-with-unshift board #,indices #,indices))
  (body))

(: board-move-down Board-Move-Direction-Proc)
(define (board-move-down board)
  (define-syntax (body stx)
    (define (->indices proc)
      (datum->syntax #'board-move-down
                     (board-indices-map proc board-indices)))
    #`(do-board-move-with-unshift
       board
       #,(->indices (λ (i j)
                      (list (index4-reverse j) i)))
       #,(->indices (λ (i j)
                      (list j (index4-reverse i))))))
  (body))






#lang racket/base

(require pict
         pict/conditional
         racket/draw
         racket/contract
         (only-in "kernel.rkt"
                  2048-game?
                  game->board
                  game-winning?
                  game-over?
                  game-score)
         "pict/utils.rkt")

(module+ test
  (require rackunit))

(provide guard-font-size
         (contract-out
          [game/w/h->window-pict
           (-> 2048-game? (>=/c 0) (>=/c 0) pict?)]
          [the-2048-tile-pict
           pict?]
          [make-window-background
           (-> (>=/c 0) (>=/c 0) pict?)]
          [game->pict
           (-> 2048-game? pict?)]))

(define (game/w/h->window-pict game w h)
  (ct-superimpose (make-window-background w h)
                  (scale-to-fit (game->pict game) w h)))

(define (game->pict game)
  (make-game-pict #:board (game->board game)
                  #:score (game-score game)
                  #:winning? (game-winning? game)
                  #:game-over? (game-over? game)))

(define (make-game-pict #:board board
                        #:score score
                        #:winning? winning?
                        #:game-over? game-over?)
  (vl-append (make-title/score-pict score
                                    #:winning? winning?
                                    #:game-over? game-over?)
             (board->pict board)))

(define (make-window-background w h)
  (filled-rectangle w h
                    #:draw-border? #f
                    #:color window-background-color))

;; Sizes:

(define board-size
  450)

(define tile-size
  (* 2/9 board-size))

(define offset-size
  (* 1/45 board-size))

(define tile-max-text-width
  (* 9/10 tile-size))

(define tile-font-size
  (guard-font-size (* 2/3 tile-size)))

(define score-rect-width
  (+ (* 2 tile-size) offset-size))

(define score-rect-height
  (* 1/2 tile-size))

(define score-rect-font-size
  (guard-font-size (* 2/3 score-rect-height)))


;                  
;                  
;                  
;                  
;   ;;  ;; ;;      
;   ;;     ;;      
;  ;;;;;;; ;;  ;;  
;   ;;  ;; ;; ;  ; 
;   ;;  ;; ;; ;  ; 
;   ;;  ;; ;;;;;;;;
;   ;;  ;; ;; ;    
;    ;  ;; ;; ;    
;    ;;;;;  ;  ;;; 
;                  
;                  
;                  
;                  


(define (make-nonempty-tile-text n)
  (scale/max-width (2048-text (number->string (expt 2 n))
                              tile-font-size
                              #:color (tile-value->text-color n))
                   tile-max-text-width))

(define (do-make-tile-pict v)
  (define bkgd
    (filled-rounded-square tile-size
                           (tile-value->background-color v)))
  (if v
      (cc-superimpose bkgd (make-nonempty-tile-text v))
      bkgd))

(define tile-value->pict
  (tile-value-prepared-lambda
      #:for [#f 1 2 3 4 5 6 7 8 9 10 11 12 13 14]
    do-make-tile-pict))

(define the-2048-tile-pict
  (tile-value->pict 11))

;                                   
;                                   
;                                   
;                                   
;   ;;                            ;;
;   ;;                            ;;
;   ;;;;    ;;;    ;;    ;; ;  ;;;;;
;   ;;  ;  ;   ;  ;  ;   ;;;  ;   ;;
;   ;;  ;  ;   ;     ;;  ;;   ;   ;;
;   ;;  ;;;;   ;;  ;;;;  ;;  ;;   ;;
;   ;;  ;  ;   ;  ;  ;;  ;;   ;   ;;
;   ;;  ;  ;   ; ;;  ;;  ;;   ;   ;;
;   ; ;;    ;;;   ;;; ;  ;;    ;;; ;
;                                   
;                                   
;                                   
;                                   


(define board-background
  (filled-rounded-square board-size border-color))

(define (board->pict board)
  (for/fold ([base board-background])
            ([i (in-naturals)]
             [row (in-vector board)]
             #:when #t
             [j (in-naturals)]
             [tile-val (in-vector row)])
    (pin-over base
              (+ offset-size
                 (* j (+ offset-size tile-size)))
              (+ offset-size
                 (* i (+ offset-size tile-size)))
              (tile-value->pict tile-val))))

(define sample-board
  #(#[1 2 3 4]
    #[5 6 7 8]
    #[9 10 11 12]
    #[#f #f #f #f]))


;                                                        
;                                                        
;                                                        
;                                   ;                    
;                                  ;  ;;  ;; ;;  ;;      
;                                  ;  ;;     ;;  ;;      
;    ;; ; ;;;   ;;;   ;; ;  ;;     ; ;;;;;;;;;;;;;;  ;;  
;  ;;  ; ;   ; ;   ;  ;;;  ;  ;   ;   ;;  ;; ;;  ;; ;  ; 
;   ;    ;     ;   ;  ;;   ;  ;   ;   ;;  ;; ;;  ;; ;  ; 
;    ;; ;;    ;;   ;; ;;  ;;;;;;  ;   ;;  ;; ;;  ;;;;;;;;
;      ;;;     ;   ;  ;;   ;      ;   ;;  ;; ;;  ;; ;    
;  ;   ; ;   ; ;   ;  ;;   ;     ;     ;  ;;  ;  ;; ;    
;   ;;;   ;;;   ;;;   ;;    ;;;  ;     ;;;;;  ;;; ;  ;;; 
;                                ;                       
;                               ;                        
;                                                        
;                                                        


(define title-pict
  (cb-superimpose
   (blank score-rect-width 1)
   (scale/max-width (trim-descent (2048-text "2048" tile-size)
                                  offset-size)
                    score-rect-width)))

(define you-win-pict
  (trim-descent (2048-text "You Win!" score-rect-font-size #:bold? #f)
                offset-size))

(define game-over-pict
  (trim-descent (2048-text "Game Over"
                           score-rect-font-size
                           #:bold? #f
                           #:color (make-color 255 0 0))
                offset-size))

(define score-rect-background
  (filled-rounded-rectangle score-rect-width score-rect-height
                            10 ;; radius
                            #:color (tile-value->background-color #f)
                            #:draw-border? #f))

(define score-prefix-text-pict
  (2048-text "Score:" score-rect-font-size #:bold? #f))

(define score-prefix-gap-size
  (* 1/8 (pict-width score-prefix-text-pict)))

(define (score->pict score)
  (define txt
    (scale/max-width
     (hbl-append score-prefix-gap-size
                 score-prefix-text-pict
                 (2048-text (number->string score) score-rect-font-size))
     (* 8/10 score-rect-width)))
  (pin-over score-rect-background
            (* 1/10 score-rect-width)
            (* 1/2 (- score-rect-height (pict-height txt)))
            txt))

(define (make-title/score-pict score #:winning? [winning? #t] #:game-over? [game-over? #t])
  (define win+game-over-pict
    (pict-cond #:combine cb-superimpose
               [(and winning? game-over?)
                (vc-append game-over-pict you-win-pict)]
               [winning? you-win-pict]
               [game-over? game-over-pict]
               [else (blank)]))
  (define rhs
    (vc-append (vc-append win+game-over-pict
                          (score->pict score))
               (blank score-rect-width offset-size)))
  (define v-blank
    (blank offset-size (+ (* 2 offset-size) score-rect-height)))
  (hbl-append v-blank title-pict v-blank rhs v-blank))


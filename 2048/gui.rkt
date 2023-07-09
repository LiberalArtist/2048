#lang racket/base

(require "state.rkt"
         "pict.rkt"
         "include-agpl.rkt"
         "gui/utils.rkt"
         "gui/preferences-redirect.rkt"
         racket/gui/base
         framework
         string-constants
         racket/cmdline
         racket/symbol
         racket/dict
         racket/match
         racket/path
         racket/class
         racket/math
         pict)

(provide main)

;; FIXME: when a new file is saved the first time,
;; it should be added to the recents list.

;; To investigate:
#;autosave:autosavable<%>
#;frame:setup-size-pref
#;tab-panel%

(define (main [argv (current-command-line-arguments)])
  ;; "On Windows, when the application is **not** running and
  ;; user double-clicks an application-handled file or drags
  ;; a file onto the application’s icon, the filename is provided
  ;; as a command-line argument to the application."
  (define files-to-open
    (command-line #:argv argv #:args files-to-open files-to-open))
  ;; ----------------------------------------
  ;; Set parameters:
  (define (set-parameters)
    (application:current-app-name "2048")
    (finder:default-filters the-default-filters)
    (finder:default-extension the-default-extension)
    (install-redirected-preferences-parameters)
    (handler:current-create-new-window create-new-window))
  (set-parameters)
  (unless (equal? (current-thread) (eventspace-handler-thread (current-eventspace)))
    (queue-callback set-parameters))
  ;; ----------------------------------------
  ;; Set global handlers:
  ;; We intentionally don't set `application-start-empty-handler`.
  ;; (That's what DrRacket seems to do, too.)
  (application-about-handler show-about-frame)
  (application-preferences-handler preferences-handler)
  (application-file-handler create-new-window)
  ;; ----------------------------------------
  ;; Start:
  (for ([file (in-list files-to-open)])
    (queue-callback
     (λ ()
       (create-new-window file))))
  (queue-callback
   (λ ()
     (unless (find-frame game-frame%)
       (create-new-window)))
   ;; low priority to go after application-file-handler
   #f)
  (void))


;                                                                       
;                                                                       
;                                                                       
;                                                                       
;                             ;;;                              ;    ;   
;                            ;;                               ; ;   ;   
;    ;;    ; ;;   ; ;;      ;;;; ;; ;  ;;    ; ;; ;;;    ;;  ;; ;; ;    
;   ;  ;   ;;  ;  ;;  ;      ;;  ;;;  ;  ;   ;; ;;  ;   ;  ;  ; ;  ; ;  
;      ;;  ;;  ;  ;;  ;      ;;  ;;      ;;  ;; ;;  ;;  ;  ;  ; ; ; ; ; 
;    ;;;;  ;;  ;; ;;  ;; ;;; ;;  ;;    ;;;;  ;; ;;  ;; ;;;;;;  ; ;  ; ; 
;   ;  ;;  ;;  ;  ;;  ;      ;;  ;;   ;  ;;  ;; ;;  ;;  ;        ; ;; ;;
;  ;;  ;;  ;;  ;  ;;  ;      ;;  ;;  ;;  ;;  ;; ;;  ;;  ;       ;   ; ; 
;   ;;; ;  ;;;;   ;;;;       ;;  ;;   ;;; ;  ;; ;;  ;;   ;;;    ;    ;  
;          ;;     ;;                                                    
;          ;;     ;;                                                    
;          ;;     ;;                                                    
;                                                                       

(define app-frame%
  (class (frame:standard-menus-mixin
          (frame:register-group-mixin
           (frame:basic-mixin frame%)))
    (inherit get-menu-item%
             set-icon)
    (super-new)
    (set-icon the-small-frame-icon-bitmap #f 'small)
    (set-icon the-large-frame-icon-bitmap #f 'large)
    ;; ----------------------------------------
    ;; Methods:
    (define/override (file-menu:new-callback item control)
      (create-new-window))
    (define/override (on-drop-file path-str)
      (create-new-window path-str))
    (define/override (file-menu:open-callback item control)
      (parameterize ([finder:default-filters the-default-filters]
                     [finder:default-extension the-default-extension])
        ;; ??? maybe be less indirect?
        (super file-menu:open-callback item control)))
    (define/override (file-menu:open-recent-on-demand menu)
      (with-redirected-preferences
       (super file-menu:open-recent-on-demand menu)))
    (define/override (edit-menu:create-preferences?)
      (super edit-menu:create-preferences?))
    (define/override (edit-menu:preferences-callback item control)
      (preferences-handler this))
    (define/override-final (help-menu:create-about?)
      #t)
    (define/override-final (help-menu:about-callback item control)
      (show-about-frame))
    (define/overment (help-menu:after-about m)
      (new (get-menu-item%)
           [label view-agpl-label]
           [parent m]
           [callback (λ (m e) (show-agpl-frame))])
      (inner (void) help-menu:after-about m))
    #|END app-frame%|#))




(define auxiliary-frame%
  (class app-frame%
    (super-new)
    (inherit close)
    (define/override (on-subwindow-char receiver event)
      (cond
        [(equal? (send event get-key-code) 'escape)
         (close)]
        [else
         (super on-subwindow-char receiver event)]))
    (define/override (file-menu:create-revert?)
      #f)
    #|END auxiliary-frame%|#))


;                                                                          
;                                                                          
;                                                                          
;                                                                          
;                        ;;      ;;;                              ;    ;   
;                        ;;     ;;                               ; ;   ;   
;    ;;     ;;;;; ; ;;   ;;    ;;;; ;; ;  ;;    ; ;; ;;;    ;;  ;; ;; ;    
;   ;  ;   ;  ;   ;;  ;  ;;     ;;  ;;;  ;  ;   ;; ;;  ;   ;  ;  ; ;  ; ;  
;      ;; ;;  ;;  ;;  ;  ;;     ;;  ;;      ;;  ;; ;;  ;;  ;  ;  ; ; ; ; ; 
;    ;;;;  ;  ;   ;;  ;; ;; ;;; ;;  ;;    ;;;;  ;; ;;  ;; ;;;;;;  ; ;  ; ; 
;   ;  ;;   ;;    ;;  ;  ;;     ;;  ;;   ;  ;;  ;; ;;  ;;  ;        ; ;; ;;
;  ;;  ;; ;;      ;;  ;  ;;     ;;  ;;  ;;  ;;  ;; ;;  ;;  ;       ;   ; ; 
;   ;;; ;  ;;;;;  ;;;;    ;     ;;  ;;   ;;; ;  ;; ;;  ;;   ;;;    ;    ;  
;          ;   ;; ;;                                                       
;         ;;   ;  ;;                                                       
;           ;;;   ;;                                                       
;                                                                          

(define agpl-frame%
  (class (frame:compound-label-mixin auxiliary-frame%)
    (inherit close get-area-container)
    (super-new [label "GNU Affero General Public License"]
               [width 500] ;; FIXME compute?
               [height 500])
    (define ed
      (new text:for-display%))
    (define ec
      (new editor-canvas%
           [parent (get-area-container)]
           [editor ed]
           [style '(auto-hscroll auto-vscroll)]))
    (send ed insert agpl 0 'same #f)
    (send ec set-line-count 10)
    (send ed lock #t)))


;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  
;          ;;                   ;;       ;;;                              ;    ;   
;          ;;                   ;;      ;;                               ; ;   ;   
;    ;;    ;;;;    ;;;   ;; ;; ;;;;;   ;;;; ;; ;  ;;    ; ;; ;;;    ;;  ;; ;; ;    
;   ;  ;   ;;  ;  ;   ;  ;; ;;  ;;      ;;  ;;;  ;  ;   ;; ;;  ;   ;  ;  ; ;  ; ;  
;      ;;  ;;  ;  ;   ;  ;; ;;  ;;      ;;  ;;      ;;  ;; ;;  ;;  ;  ;  ; ; ; ; ; 
;    ;;;;  ;;  ;;;;   ;; ;; ;;  ;;  ;;; ;;  ;;    ;;;;  ;; ;;  ;; ;;;;;;  ; ;  ; ; 
;   ;  ;;  ;;  ;  ;   ;  ;; ;;  ;;      ;;  ;;   ;  ;;  ;; ;;  ;;  ;        ; ;; ;;
;  ;;  ;;  ;;  ;  ;   ;   ; ;;   ;      ;;  ;;  ;;  ;;  ;; ;;  ;;  ;       ;   ; ; 
;   ;;; ;  ; ;;    ;;;    ;;;;   ;;;    ;;  ;;   ;;; ;  ;; ;;  ;;   ;;;    ;    ;  
;                                                                                  
;                                                                                  
;                                                                                  
;                                                                                  


(define about-frame%
  ;; modeled after drracket/private/app.rkt
  (class (forbid-set-label-mixin auxiliary-frame%)
    (inherit close get-area-container)
    (super-new [label "About 2048"])
    ;; TODO: maybe override place-children in the (get-area-container)
    ;; to put some extra space up to a limit at the top when stretched.
    (new message%
         [parent (get-area-container)]
         [label (pict->screen-bitmap the-2048-tile-pict)])
    (let ()
      ;; TODO maybe use a text% ?
      (define byline-font
        (send the-font-list
              find-or-create-font
              (guard-font-size (* 4/3 (send normal-control-font get-size)))
              (send normal-control-font get-family)
              (send normal-control-font get-style)
              'bold
              (send normal-control-font get-underlined)
              (send normal-control-font get-smoothing)
              (send normal-control-font get-size-in-pixels)
              (send normal-control-font get-hinting)))
      (define pict
        (hbl-append (text "By Philip M" byline-font)
                    (text "c" (cons 'superscript byline-font))
                    (text "Grath" byline-font)))
      (new message%
           [parent (get-area-container)]
           [label (pict->screen-bitmap pict)]))
    (let ()
      (define ed
        (new text:for-display%
             [auto-wrap #t]))
      (define ec
        (new editor-canvas%
             [parent (get-area-container)]
             [editor ed]
             [min-width (* 3 (exact-ceiling (pict-width the-2048-tile-pict)))]
             [stretchable-width #f]
             [stretchable-height #f]
             [style '(transparent no-border no-hscroll no-vscroll)]))
      (send ed
            insert
            (string-append-immutable
             "This program is free software, and you are welcome"
             " to redistribute it"
             " under the terms of the GNU Affero General Public License."
             " The program comes with absolutely no waranty:" ;; bold?
             " see the GNU Affero General Public License for details.")
            0
            'same
            #f)
      (send (get-area-container) reflow-container)
      (send ed lock #t)
      (send ec set-line-count (add1 (send ed last-line))))
    (new button%
         [parent (get-area-container)]
         [label view-agpl-label]
         [callback (λ (b e) (show-agpl-frame))])
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    (define/override (file-menu:create-save?) #f)
    (define/override (file-menu:create-save-as?) #f)
    #|END about-frame%|#))

;                                                                                 
;                                                                                 
;                                                                                 
;                                                                                 
;                                       ;;;                              ;    ;   
;                                      ;;                               ; ;   ;   
;    ;;;;;  ;;    ; ;; ;;;    ;;      ;;;; ;; ;  ;;    ; ;; ;;;    ;;  ;; ;; ;    
;   ;  ;   ;  ;   ;; ;;  ;   ;  ;      ;;  ;;;  ;  ;   ;; ;;  ;   ;  ;  ; ;  ; ;  
;  ;;  ;;     ;;  ;; ;;  ;;  ;  ;      ;;  ;;      ;;  ;; ;;  ;;  ;  ;  ; ; ; ; ; 
;   ;  ;    ;;;;  ;; ;;  ;; ;;;;;; ;;; ;;  ;;    ;;;;  ;; ;;  ;; ;;;;;;  ; ;  ; ; 
;    ;;    ;  ;;  ;; ;;  ;;  ;         ;;  ;;   ;  ;;  ;; ;;  ;;  ;        ; ;; ;;
;  ;;     ;;  ;;  ;; ;;  ;;  ;         ;;  ;;  ;;  ;;  ;; ;;  ;;  ;       ;   ; ; 
;   ;;;;;  ;;; ;  ;; ;;  ;;   ;;;      ;;  ;;   ;;; ;  ;; ;;  ;;   ;;;    ;    ;  
;   ;   ;;                                                                        
;  ;;   ;                                                                         
;    ;;;                                                                          
;                                                                                 

(define game-frame%
  (class (frame:compound-label:trim-and-set-mixin app-frame%)
    (inherit get-area-container
             get-menu-bar
             get-menu%
             get-menu-item%
             get-label)
    (init [(:state state)])
    (define state :state)
    (super-new [width 450]
               [height 450]
               [label (cond
                        [(state-get-maybe-path state)
                         => (λ (pth) (path->base-label pth))]
                        [else
                         (gui-utils:next-untitled-name)])])
    ;; ---------------------------------------------------------
    ;; Bookmarks Menu:
    (define-values [m-auto-bookmarks m-custom-bookmarks]
      (let ()
        (define m-bookmarks
          (new (get-menu%)
               ;; TODO: should it be "&Bookmarks"?
               [label "Bookmarks"]
               [parent (get-menu-bar)]))
        #|
        ;; TODO:
        (new (get-menu-item%)
             [label "Show All Bookmarks"]
             [parent m-bookmarks]
             [callback (λ (item control)
                         ;; TODO
                         (void))])
        (new separator-menu-item%
             [parent m-bookmarks])
        |#
        (new (get-menu-item%)
             [label "Set Bookmark"]
             [parent m-bookmarks]
             [shortcut #\b]
             [callback (λ (item control)
                         (do-set-auto-bookmark))])
        (new (get-menu-item%)
             [label "Set Custom Bookmark…"]
             [parent m-bookmarks]
             [callback (λ (item control)
                         (do-set-custom-bookmark))])
        (new separator-menu-item%
             [parent m-bookmarks])
        (define m-auto-bookmarks
          (new (get-menu%)
               [label "Automatic Bookmarks"]
               [parent m-bookmarks]))
        (define m-custom-bookmarks
          (new (get-menu%)
               [label "Custom Bookmarks"]
               [parent m-bookmarks]))
        (values m-auto-bookmarks m-custom-bookmarks)))
    ;; ---------------------------------------------------------
    ;; Canvas & Event Listeners:
    (let ()
      (define the-canvas
        (new canvas%
             [parent (get-area-container)]
             [paint-callback
              (λ (canvas dc)
                (define-values [w h]
                  (send canvas get-client-size))
                (define game
                  (state-get-game state))
                (draw-pict (game/w/h->window-pict game w h) dc 0 0))]))
      (define game-change-evt
        (wrap-evt (state-subscribe/board* state)
                  (λ (__)
                    (queue-callback
                     (λ () (send the-canvas refresh-now))))))
      (define bookmarks-change-evt
        (wrap-evt (state-subscribe/bookmarks state)
                  (λ (__)
                    (queue-callback
                     (λ () (install-bookmarks!))))))
      (define fs-change-evt
        (wrap-evt (state-subscribe/file state)
                  (λ (__)
                    ;; doesn't need queue-callback
                    (instal-updated-label!))))
      (thread (λ ()
                (let loop ()
                  (sync game-change-evt
                        bookmarks-change-evt
                        fs-change-evt)
                  (loop)))))
    ;; ---------------------------------------------------------
    ;; Finish initialization:
    (frame:reorder-menus this)
    (install-bookmarks!)
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;                                                 
    ;                                                 
    ;                                                 
    ;                                                 
    ;                    ;;  ;;                ;;     
    ;                    ;;  ;;                ;;     
    ;   ; ;; ;;;    ;;  ;;;;;;;;;;   ;;;    ;;;;;  ;; 
    ;   ;; ;;  ;   ;  ;  ;;  ;;  ;  ;   ;  ;   ;;;;  ;
    ;   ;; ;;  ;;  ;  ;  ;;  ;;  ;; ;   ;  ;   ;; ;   
    ;   ;; ;;  ;; ;;;;;; ;;  ;;  ;;;;   ;;;;   ;;  ;; 
    ;   ;; ;;  ;;  ;     ;;  ;;  ;; ;   ;  ;   ;;    ;
    ;   ;; ;;  ;;  ;      ;  ;;  ;; ;   ;  ;   ;;;   ;
    ;   ;; ;;  ;;   ;;;   ;;;;;  ;;  ;;;    ;;; ; ;;; 
    ;                                                 
    ;                                                 
    ;
    ;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;                                                         
    ;                                                         
    ;                                                         
    ;                                                         
    ;        ;;      ;;                                       
    ;        ;;      ;;                                       
    ;    ;; ;;;;; ;;;;;    ; ;; ;;;    ;;   ; ;;;  ;; ;;   ;; 
    ;  ;;  ; ;;  ;   ;;    ;; ;;  ;   ;  ;  ;;  ;  ;; ;; ;;  ;
    ;   ;    ;;  ;   ;;    ;; ;;  ;;  ;  ;  ;;  ;; ;; ;;  ;   
    ;    ;;  ;; ;;   ;;    ;; ;;  ;; ;;;;;; ;;  ;; ;; ;;   ;; 
    ;      ;;;;  ;   ;;    ;; ;;  ;;  ;     ;;  ;; ;; ;;     ;
    ;  ;   ;  ;  ;   ;;    ;; ;;  ;;  ;     ;;  ;;  ; ;; ;   ;
    ;   ;;;   ;;; ;;; ;    ;; ;;  ;;   ;;;  ;;  ;;  ;;;;  ;;; 
    ;                                                         
    ;                                                         
    ;                                                         
    ;                                                         
    (define/override-final (file-menu:create-revert?)
      #t)
    (define/override-final (file-menu:revert-on-demand item)
      (send item enable (state-get-maybe-path state)))
    (define/override-final (file-menu:revert-callback item control)
      (when (user-oks-revert this)
        (match (state-revert! state)
          ['ok
           (void)]
          [(cons e pth)
           (message-box/open-file-error
            e
            (send (group:get-the-frame-group) locate-file pth))])))
    (define/override-final (file-menu:create-save?)
      #t)
    (define/override-final (file-menu:save-callback item control)
      (do-save))
    (define/override-final (file-menu:create-save-as?)
      #t)
    (define/override-final (file-menu:save-as-callback item control)
      (do-save-as))
    (define/override (file-menu:create-print?)
      ;; TODO: see also file-menu:between-save-as-and-print
      ;; from frame:editor-mixin, which creates a Print Setup.
      #f)
    (define/override (file-menu:print-callback item control)
      (super file-menu:print-callback item control))
    (define/override-final (edit-menu:undo-callback item control)
      (state-undo! state))
    (define/override-final (edit-menu:undo-on-demand item)
      (send item enable (state-get-can-undo? state)))
    (define/override-final (edit-menu:redo-callback item control)
      (state-redo! state))
    (define/override-final (edit-menu:redo-on-demand item)
      (send item enable (state-get-can-redo? state)))
    (define/override-final (edit-menu:create-cut?)
      #f)
    (define/override-final (edit-menu:create-copy?)
      #f)
    (define/override-final (edit-menu:create-paste?)
      #f)
    (define/override-final (edit-menu:create-clear?)
      #f)
    (define/override-final (edit-menu:create-select-all?)
      #f)    
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
    (define/private (do-set-auto-bookmark)
      (define key (state-set-auto-bookmark! state))
      (message-box "Bookmark Created"
                   (string-append
                    "Created automatic bookmark "
                    (number->string key)
                    ".")
                   this
                   '(ok))
      (void))
    (define/private (do-set-custom-bookmark)
      (define (user-wants-to-rety? msg)
        (= 1 (message-box/custom "Invalid Bookmark Name"
                                 msg
                                 "Choose another name"
                                 (string-constant cancel)
                                 #f
                                 this
                                 '(stop default=1)
                                 2)))
      (let retry ([init-val ""])
        (match (get-text-from-user
                "Set Custom Bookmark"
                "Enter a name for this bookmark:"
                this
                init-val)
          [#f
           (void)]
          [(pregexp #px"^\\s*$")
           (when (user-wants-to-rety?
                  "The bookmark’s name must not be blank.")
             (retry ""))]
          [str
           (define sym (string->symbol str))
           (unless (state-try-set-custom-bookmark! state sym)
             (when (user-wants-to-rety?
                    (string-append "A bookmark named “"
                                   str
                                   "” already exists."))
               (retry str)))])))
    (define/private (install-bookmarks!)
      (define (menu-delete-items menu)
        (for ([item (in-list (send menu get-items))])
          (send item delete)))
      (define-values [max-auto custom]
        (state-get-all-bookmarks state))
      (menu-delete-items m-auto-bookmarks)
      (menu-delete-items m-custom-bookmarks)
      (cond
        [max-auto
         (send m-auto-bookmarks enable #t)
         (for ([i (in-range 0 (add1 max-auto))])
           (new (get-menu-item%)
                [parent m-auto-bookmarks]
                [label (gui-utils:quote-literal-label
                        (number->string i))]
                [callback (λ (item control)
                            (do-goto-bookmark i))]))]
        [else
         (send m-auto-bookmarks enable #f)])
      (cond
        [(null? custom)
         (send m-custom-bookmarks enable #f)]
        [else
         (send m-custom-bookmarks enable #t)
         (for ([{sym str} (in-dict (sort (map (λ (sym)
                                                (cons sym (symbol->immutable-string sym)))
                                              custom)
                                         #:key cdr
                                         string-locale-ci<?))])
           (new (get-menu-item%)
                [parent m-custom-bookmarks]
                [label (gui-utils:quote-literal-label str)]
                [callback (λ (item control)
                            (do-goto-bookmark sym))]))]))
    (define/private (do-goto-bookmark k)
      ;; TODO:
      ;;   - maybe offer to set automatic bookmark on departure?
      ;;   - don't show again?
      (when (= 1 (message-box/custom
                  "Go To Bookmark"
                  (string-append-immutable
                   "Are you sure you want to go to "
                   (if (symbol? k)
                       (string-append-immutable
                        "the bookmark “"
                        (symbol->immutable-string k)
                        "”")
                       (string-append-immutable
                        "bookmark " (number->string k)))
                   "?\n\nThis action cannot currently be undone.")
                  "Go"
                  (string-constant cancel)
                  #f
                  this
                  '(default=1 caution)
                  2))
        (state-goto-bookmark! state k)))
    ;                            
    ;                            
    ;                            
    ;                            
    ;   ;;        ;;           ;;
    ;   ;;        ;;           ;;
    ;   ;;  ;;    ;;;;    ;;   ;;
    ;   ;; ;  ;   ;;  ;  ;  ;  ;;
    ;   ;;    ;;  ;;  ;  ;  ;  ;;
    ;   ;;  ;;;;  ;;  ;;;;;;;; ;;
    ;   ;; ;  ;;  ;;  ;  ;     ;;
    ;   ;;;;  ;;  ;;  ;  ;     ;;
    ;    ; ;;; ;  ; ;;    ;;;   ;
    ;                            
    ;                            
    ;                            
    ;                            
    (define/private (path->base-label pth)
      (path->string (or (file-name-from-path pth) pth)))
    (define/private (instal-updated-label!)
      (define maybe-path
        (state-get-maybe-path state))
      (when maybe-path
        (super trim-and-set-label
               (path->base-label maybe-path))))
    (define/override-final (trim-and-set-label v)
      (raise-arguments-error (object-name this%)
                             "trim-and-set-label method not allowed"
                             "given" v))
    ;                          
    ;                          
    ;                          
    ;                          
    ;                          
    ;                          
    ;    ;; ; ;;   ;    ;  ;;  
    ;  ;;  ; ;  ;   ;  ;  ;  ; 
    ;   ;       ;;  ;  ;  ;  ; 
    ;    ;;   ;;;;  ;  ; ;;;;;;
    ;      ;;;  ;;   ; ;  ;    
    ;  ;   ;;;  ;;   ;;   ;    
    ;   ;;;  ;;; ;   ;;    ;;; 
    ;                          
    ;                          
    ;                          
    ;                          
    (define/private (do-save)
      (cond
        [(state-get-maybe-path state)
         => (λ (pth)
              (finish-doing-save/as pth))]
        [else
         (do-save-as)]))
    (define/private (do-save-as)
      (define-values [default-name dir]
        (cond
          [(state-get-maybe-path state)
           => (λ (pth)
                (values (file-name-from-path pth) (path-only pth)))]
          [else
           (values #f #f)]))
      (define dest
        (put-file (string-constant select-file)
                  this dir default-name the-default-extension null the-default-filters))
      (when dest
        (finish-doing-save/as dest)))
    (define/private (finish-doing-save/as dest)
      (with-handlers ([exn:fail?
                       ;; based on framework/private/editor
                       (λ (exn)
                         (message-box
                          (string-constant error-saving)
                          (string-append
                           (format (string-constant error-saving-file/name) 
                                   dest)
                           "\n\n"
                           (exn-message exn))
                          this
                          '(stop ok))
                         (void))])
        (state-save! state dest)))
    ;                           
    ;                           
    ;                           
    ;                           
    ;              ;;           
    ;                           
    ;   ; ;; ;;;   ;;  ;; ; ;;; 
    ;   ;; ;;  ;   ;;;;  ; ;   ;
    ;   ;; ;;  ;;  ;; ;    ;    
    ;   ;; ;;  ;;  ;;  ;; ;;    
    ;   ;; ;;  ;;  ;;    ;;;    
    ;   ;; ;;  ;;  ;;;   ; ;   ;
    ;   ;; ;;  ;;  ;; ;;;   ;;; 
    ;                           
    ;                           
    ;                           
    ;                           
    (define/override (on-subwindow-char receiver evt)
      (match (send evt get-key-code)
        [(and dir (or 'left 'right 'up 'down))
         (state-move! state dir)]
        [_ (super on-subwindow-char receiver evt)]))
    (define/override-final (get-filename [temp #f])
      (when temp
        ;; change if autosave
        (set-box! temp #f))
      (state-get-maybe-path state))
    (define/override-final (editing-this-file? filename)
      (define (path-equal? x y)
        ;; based on frame:editor-mixin
        (with-handlers ([exn:fail? (λ (e) #f)])
          (equal? (normal-case-path (normalize-path x))
                  (normal-case-path (normalize-path y)))))
      (define maybe-path (state-get-maybe-path state))
      (and maybe-path
           (equal? filename maybe-path)))
    (define/augment-final (can-close?)
      ;; see can-close? in editor:file-mixin
      (or (not (state-get-has-unsaved-changes? state))
          (match (gui-utils:unsaved-warning (get-label)
                                            (string-constant dont-save)
                                            #t
                                            this
                                            #t)
            ['continue
             #t]
            ['cancel
             #f]
            ['save
             (do-save)
             ;; options on failure?
             #t])))
    #|END game-frame%|#))



#;
(define/augment (can-save-file? filename format)
  (and (if (equal? filename (get-filename))
           (if (save-file-out-of-date?)
               (gui-utils:get-choice
                (string-constant file-has-been-modified)
                (string-constant overwrite-file-button-label)
                (string-constant cancel)
                (string-constant warning)
                #f
                (get-top-level-window)
                #:dialog-mixin frame:focus-table-mixin)
               #t)
           #t)
       (inner #t can-save-file? filename format)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(define the-default-filters
  '(["2048 Game" "*.2048"]))
(define the-default-extension
  "2048")

(define (preferences-handler [parent #f])
  ;; workaround for https://github.com/racket/gui/issues/170
  (message-box "Preferences - 2048"
               "2048 doesn't currently support any preferences."
               parent)
  (void))

(define view-agpl-label
  "View GNU Affero General Public License")

(define (show-agpl-frame)
  (reveal-or-create-frame agpl-frame%))

(define (show-about-frame)
  (reveal-or-create-frame about-frame%))

(define (create-new-window [maybe-path #f])
  (with-handlers ([exn:fail? message-box/open-file-error])
    (define frame
      (let* ([state (make-state maybe-path)]
             [maybe-path (state-get-maybe-path state)])
        (when maybe-path
          (with-redirected-preferences
           (handler:add-to-recent maybe-path)))
        (new game-frame% [state state])))
    (send frame show #t)
    frame))

(define the-small-frame-icon-bitmap
  (pict->screen-bitmap (scale-to-fit the-2048-tile-pict 16 16)))
(define the-large-frame-icon-bitmap
  (pict->screen-bitmap (scale-to-fit the-2048-tile-pict 32 32)))


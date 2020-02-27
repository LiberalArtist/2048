#lang racket

(require "pict.rkt"
         "pict/file-icon.rkt"
         (except-in pict file-icon)
         racket/draw
         racket/runtime-path
         file/convertible
         file/ico
         icns)

(define-runtime-path-list all-icon-paths
  (map (λ (p) (build-path "main" "gui" p))
       '("2048.png" "2048.ico" "2048.icns" "doc.png" "doc.ico" "doc.icns")))

(match-define (list icon.png icon.ico icon.icns
                    doc.png doc.ico doc.icns)
  all-icon-paths)

(define (write-icons)
  (write-icon-variants the-2048-tile-pict
                       #:png icon.png #:ico icon.ico #:icns icon.icns)
  (write-icon-variants document-icon
                       #:png doc.png #:ico doc.ico #:icns doc.icns))

(define document-icon
  (panorama
   (cc-superimpose
    (blank 203 203)
    (pin-over
     (colorize
      (linewidth 2 (file-icon
                    #:height 200
                    #:brush (make-brush #:color "white")))
      (make-color 125 125 125))
     (- 100/3)
     (- 200 200/3)
     (scale-to-fit the-2048-tile-pict 100 100)))))

(define (write-icon-variants pict #:png png-pth #:ico ico-pth #:icns icns-pth)
  (define png-bytes
    (convert (scale-to-fit pict 96 96) 'png-bytes))
  (define icos
    (for/list ([size (in-list '(256 128 64 48 32 16))])
      (argb->ico size size
                 (pict->argb-pixels
                  (cc-superimpose (blank size size)
                                  (scale-to-fit pict size size))))))
  (define icns-bytes
    (pict->icns-bytes pict))
  (call-with-output-file* png-pth
    #:exists 'truncate/replace
    (λ (out) (write-bytes png-bytes out)))
  (write-icos icos ico-pth #:exists 'truncate/replace)
  (call-with-output-file* icns-pth
    #:exists 'truncate/replace
    (λ (out) (write-bytes icns-bytes out)))
  (void))
               
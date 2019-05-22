#lang racket/base
(require (file "link-analysis.rkt")
         racket/pretty
         racket/gui
         racket/draw)

(define (take-while proc lst)
  (define (take-while-acc p l a)
    (cond [(empty? l) ; List empty?
           (values (reverse a) empty)]
          [(proc (first l) a) ; Take while the proc is true
           (take-while-acc p (rest l) (cons (first l) a))]
          [(empty? a) ; When the first element fails the proc, return it anyway
           (values (list (first l)) (rest l))]
          [else ; Proc is false, so return what we have found
           (values (reverse a) l)]))
  (take-while-acc proc lst empty))

(define (group-while proc lst)
  (cond [(empty? lst) empty]
        [else
         (define-values (l r) (take-while proc lst))
         (cons l (group-while proc r))]))

(define (words->sentences words len)
  (map string-join
       (group-while (lambda (elm acc)
                      (< (+ (string-length elm)
                            (for/sum ([w (in-list acc)])
                              (add1 (string-length w))))
                         len))
                    words)))

(define (sentence-split str len)
  (define words (string-split str))
  (string-join (words->sentences words len) "\n"))

(define (run-app)
  (define *app-width*  800)
  (define *app-height* 500)
  (define (button-callback b e)
    (define url      (send link-input get-value))
    (define analysis (link->link-analysis url))
    (update-output analysis))

  (define (update-output analysis)
    (define dc (send image get-dc))
    (set! last-drawn-bitmap (link-analysis-image analysis))
    (draw-logo image dc)
    (send out-text erase)
    (define output (format "~a~%~a~%~a"
                           (link-analysis-title analysis)
                           (link-analysis-url   analysis)
                           (sentence-split (link-analysis-description analysis)
                                           60)))
    (send out-text insert output))

  (define (draw-logo c dc)
    (send dc erase)
    (send dc draw-bitmap last-drawn-bitmap 0 0))

  ;; GUI
  (define f (new frame% [label "Link analyser"]))
  (define v (new vertical-pane%
                 [parent f]
                 [min-width *app-width*]
                 [min-height *app-height*]))

  ; Input
  (define input-area (new horizontal-pane%
                          [parent v]
                          [stretchable-height #f]))
  (define link-input (new text-field%
                          [parent input-area]
                          [label "Link:"]
                          [min-width (scale-by-ratio *app-width* 2/3)]))
  (define submit-button (new button%
                             [parent input-area]
                             [label "Analyse"]
                             [callback button-callback]))

  ; Output
  (define output-area (new horizontal-pane%
                           [parent v]))
  ; Title, url, description
  (define text-output (new vertical-pane%
                           [parent output-area]
                           [min-width (scale-by-ratio *app-width* 1/2)]))
  (define out-text (new text%))
  (define out-canvas (new editor-canvas%
                          [parent text-output]
                          [editor out-text]))

  ; Logo
  (define image-output (new vertical-pane%
                            [parent output-area]))
  (define last-drawn-bitmap (make-monochrome-bitmap 100 100))
  (define image (new canvas%
                     [parent image-output]
                     [min-width (* *app-width* 1/2)]
                     [paint-callback draw-logo]))
  (send f show #t))

(define (scale-by-ratio i r)
  (exact-round (* i r 1.0)))

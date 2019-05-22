#lang racket/base
(require net/url
         json
         racket/file
         racket/string
         racket/port
         (only-in racket/class is-a?/c)
         racket/draw
         racket/contract)

(provide (contract-out [struct link-analysis [(title       string?)
                                              (url         string?)
                                              (description string?)
                                              (image       (is-a?/c bitmap%))]]
                       [link->link-analysis  (-> string? link-analysis?)]))

(struct link-analysis (title url description image))

;; Link analysis and api key provided by https://www.linkpreview.net/

(define (link->url q)
  (string->url (format "https://api.linkpreview.net?key=~a&q=~a" (get-api-key) q)))

(define (get-api-key)
  (string-trim (file->string "./api-key.txt" #:mode 'text)))

(define (link->json q)
  (call/input-url (link->url q)
                  get-pure-port ; Strip headers
                  read-json))

(define (link->bitmap% l)
  (case l
    [("") (make-monochrome-bitmap 100 100)]
    [else (call/input-url (string->url l)
                          get-pure-port
                          read-bitmap)]))

(define-syntax string-or
  (syntax-rules ()
    [(string-or s)       s]
    [(string-or s1 s2 ...) (if (> (string-length s1) 0)
                             s1
                             (string-or s2 ...))]))

(define (json->link-analysis js)
  (define title       (string-or (hash-ref js 'title "")
                                 "Unkown title"))
  (define url         (string-or (hash-ref js 'url "")
                                 "Unkown url"))
  (define description (string-or (hash-ref js 'description "")
                                 "Unkown description"))
  (define image       (link->bitmap% (hash-ref js 'image "")))
  (link-analysis title url description image))

(define link->link-analysis
  (compose json->link-analysis link->json))

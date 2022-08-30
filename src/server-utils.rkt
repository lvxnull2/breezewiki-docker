#lang racket/base

(provide
 ext->mime-type)

(module+ test
  (require rackunit))

(define hash-ext-mime-type
  (hash #".css" #"text/css"
        #".svg" #"image/svg+xml"
        #".png" #"image/png"))
(define (ext->mime-type ext)
  (hash-ref hash-ext-mime-type ext))
(module+ test
  (check-equal? (ext->mime-type #".png") #"image/png"))

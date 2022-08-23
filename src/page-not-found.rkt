#lang racket/base

(require html-writing
         web-server/http)

(provide
 page-not-found)

(define (page-not-found req)
  (response/output
   #:code 404
   (λ (out)
     (write-html
      `(html
        (body
         (h1 "Not found.")
         (pre ,(format "~v" req))))
      out))))

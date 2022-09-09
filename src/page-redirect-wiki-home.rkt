#lang racket/base
(require net/url
         web-server/http
         "application-globals.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 redirect-wiki-home)

(define (redirect-wiki-home req)
  (response-handler
   (define wikiname (path/param-path (car (url-path (request-uri req)))))
   (define dest (format "/~a/wiki/Main_Page" wikiname))
   (generate-redirect dest)))

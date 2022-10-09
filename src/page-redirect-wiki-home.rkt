#lang racket/base
(require net/url
         web-server/http
         "application-globals.rkt"
         "data.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 redirect-wiki-home)

(define (redirect-wiki-home req)
  (response-handler
   (define wikiname (path/param-path (car (url-path (request-uri req)))))
   (define siteinfo (siteinfo-fetch wikiname))
   (define dest (format "/~a/wiki/~a" wikiname (or (siteinfo^-basepage siteinfo) "Main_Page")))
   (generate-redirect dest)))

#lang racket/base
(require racket/dict
         ; web server libs
         net/url
         web-server/http
         "application-globals.rkt"
         "data.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 page-global-search)

(define (page-global-search req)
  (define wikiname (dict-ref (url-query (request-uri req)) 'wikiname #f))
  (define q (dict-ref (url-query (request-uri req)) 'q #f))
  (response-handler
   (cond
     [(not wikiname)
      (response/output
       #:code 400
       #:mime-type "text/plain"
       (λ (out)
         (displayln "Requires wikiname and q parameters." out)))]
     [(or (not q) (equal? q ""))
      (define siteinfo (siteinfo-fetch wikiname))
      (define dest (format "/~a/wiki/~a" wikiname (or (siteinfo^-basepage siteinfo) "Main_Page")))
      (generate-redirect dest)]
     [#t
      (generate-redirect (format "/~a/search?~a"
                                 wikiname
                                 (params->query `(("q" . ,q)))))])))

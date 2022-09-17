#lang racket/base
(require racket/dict
         ; web server libs
         net/url
         web-server/http
         "application-globals.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 page-global-search)

(define (page-global-search req)
  (define wikiname (dict-ref (url-query (request-uri req)) 'wikiname #f))
  (define q (dict-ref (url-query (request-uri req)) 'q #f))
  (response-handler
   (if (not (and wikiname q))
       (response/output
        #:code 400
        #:mime-type "text/plain"
        (λ (out)
          (displayln "Requires wikiname and q parameters." out)))
       (generate-redirect (format "/~a/search?~a"
                                  wikiname
                                  (params->query `(("q" . ,q))))))))

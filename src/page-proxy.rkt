#lang racket/base
(require racket/dict
         racket/match
         racket/port
         ; libs
         (prefix-in easy: net/http-easy)
         ; web server libs
         net/url
         web-server/http
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         "application-globals.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 page-proxy)

(define (page-proxy req)
  (match (dict-ref (url-query (request-uri req)) 'dest #f)
    [(? string? dest)
     (if (is-fandom-url? dest)
         (response-handler ; catches and reports errors
          (let ([dest-r (easy:get dest #:stream? #t)])
            (with-handlers ([exn:fail? (λ (e) ; cleans up and re-throws
                                         (easy:response-close! dest-r)
                                         (raise e))])
              (response/output
               #:code (easy:response-status-code dest-r)
               #:mime-type (easy:response-headers-ref dest-r 'content-type)
               #:headers (build-headers always-headers)
               (λ (out)
                 (copy-port (easy:response-output dest-r) out)
                 (easy:response-close! dest-r))))))
         (next-dispatcher))]
    [#f (next-dispatcher)]))

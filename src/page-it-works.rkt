#lang racket/base
(require racket/dict
         net/url
         web-server/http
         web-server/dispatchers/dispatch
         "application-globals.rkt")

(provide
 page-it-works)

(define (page-it-works req)
  (define b? (dict-ref (url-query (request-uri req)) 'b #f))
  (if b?
      (generate-redirect "/stampylongnose/wiki/It_Works")
      (next-dispatcher)))

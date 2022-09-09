#lang racket/base
(require racket/path
         racket/string
         net/url
         web-server/http
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         "application-globals.rkt"
         "config.rkt"
         "xexpr-utils.rkt")

(provide
 subdomain-dispatcher)

(define (subdomain-dispatcher subdomain)
  (lift:make
   (λ (req)
     (response-handler
      (define uri (request-uri req))
      (define path (url-path uri))
      (define path-string (string-join (map (λ (p) (path/param-path p)) path) "/"))
      (define dest (format "~a/~a/~a" (config-get 'canonical_origin) subdomain path-string))
      (generate-redirect dest)))))

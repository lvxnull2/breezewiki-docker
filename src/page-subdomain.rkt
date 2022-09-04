#lang racket/base
(require racket/path
         racket/string
         net/url
         web-server/http
         web-server/servlet-dispatch
         html-writing
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
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
      (define dest-bytes (string->bytes/utf-8 dest))
      (response/output
       #:code 302
       #:headers (list (header #"Location" dest-bytes))
       (λ (out)
         (write-html
          `(html
            (head
             (title "Redirecting..."))
            (body
             "Redirecting to "
             (a (@ (href ,dest)) ,dest)
             "..."))
          out)))))))

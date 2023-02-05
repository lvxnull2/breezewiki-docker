#lang racket/base
(require racket/match
         racket/path
         racket/string
         net/url
         web-server/http
         web-server/dispatchers/dispatch
         (only-in racket/promise delay)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         "application-globals.rkt"
         "config.rkt"
         "../lib/syntax.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 subdomain-dispatcher)

(module+ test
  (require rackunit))

(define (do-redirect:make subdomain canonical-origin)
  (lift:make
   (λ (req)
     (response-handler
      (define uri (request-uri req))
      (define path (url-path uri))
      (define path-string (string-join (map (λ (p) (path/param-path p)) path) "/"))
      (define dest (format "~a/~a/~a" canonical-origin subdomain path-string))
      (generate-redirect dest)))))

(define (router req)
  (define host (bytes->string/utf-8 (header-value (headers-assq* #"host" (request-headers/raw req)))))
  (define x-canonical-origin (headers-assq* #"x-canonical-origin" (request-headers/raw req)))
  (define canonical-origin
    (cond
      [x-canonical-origin (bytes->string/utf-8 (header-value x-canonical-origin))]
      [(config-true? 'canonical_origin) (config-get 'canonical_origin)]
      [#t #f]))
  (if/out canonical-origin
          (let* ([canonical-origin-host (url-host (string->url canonical-origin))])
            (if/in canonical-origin-host
                   (let* ([splitter (string-append "." (url-host (string->url canonical-origin)))]
                          [s (string-split host splitter #:trim? #f)])
                     (if/in (and (eq? 2 (length s)) (equal? "" (cadr s)))
                            (list 'redirect (car s) canonical-origin)))))
          'next-dispatcher))
(module+ test
  (define (qr url headers)
    (request #"GET" (string->url url) (map (λ (h) (header (car h) (cadr h))) headers) (delay '()) #f "127.0.0.1" 10416 "127.0.0.1"))
  (parameterize ([(config-parameter 'canonical_origin) "https://breezewiki.com"])
    (check-equal? (router (qr "/" '((#"Host" #"breezewiki.com"))))
                  'next-dispatcher)
    (check-equal? (router (qr "/wiki/Spell" '((#"Host" #"magic.breezewiki.com"))))
                  '(redirect "magic" "https://breezewiki.com"))
    (check-equal? (router (qr "/" '((#"Host" #"magic.bw.breezewiki.com")
                                    (#"X-Canonical-Origin" #"https://bw.breezewiki.com"))))
                  '(redirect "magic" "https://bw.breezewiki.com"))
    (check-equal? (router (qr "/" '((#"Host" #"magic.bwxxxxx.onion")
                                    (#"X-Canonical-Origin" #"http://bwxxxxx.onion"))))
                  '(redirect "magic" "http://bwxxxxx.onion"))))

(define (subdomain-dispatcher conn req)
  (match (router req)
    [(list 'redirect subdomain canonical-origin) ((do-redirect:make subdomain canonical-origin) conn req)]
    [_ (next-dispatcher)]))

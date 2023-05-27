#lang racket/base
(require racket/string
         net/url
         (only-in net/uri-codec uri-decode)
         "url-utils.rkt")
(provide
 local-encoded-url->segments
 url-segments->basename
 local-encoded-url->basename
 basename->name-for-query
 url-segments->guess-title)

(define (local-encoded-url->segments str) ; '("wiki" "Page_title")
  (map path/param-path (fix-semicolons-url-path (url-path (string->url str)))))

(define (url-segments->basename segments) ; "Page_title" filename encoded, no extension or dir prefix
  (define extra-encoded (map (λ (s) (bytes->string/latin-1 (percent-encode s filename-set #f))) (cdr segments)))
  (define basic-filename (string-join extra-encoded "#"))
  basic-filename)

(define (local-encoded-url->basename str) ; '("wiki" "Page_title"), no extension or dir prefix
  (url-segments->basename (local-encoded-url->segments str)))

(define (basename->name-for-query str)
  (uri-decode (regexp-replace* #rx"#" str "/")))

(define (url-segments->guess-title segments)
  (regexp-replace* #rx"_" (cadr segments) " "))

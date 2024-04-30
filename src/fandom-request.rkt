#lang typed/racket/base
(require "config.rkt"
         "../lib/url-utils.rkt")
(define-type Headers (HashTable Symbol (U Bytes String)))
(require/typed net/http-easy
  [#:opaque Timeout-Config timeout-config?]
  [#:opaque Response response?]
  [#:opaque Session session?]
  [current-session (Parameter Session)]
  [make-timeout-config ([#:lease Positive-Real] [#:connect Positive-Real] -> Timeout-Config)]
  [get ((U Bytes String)
        [#:close? Boolean]
        [#:headers Headers]
        [#:timeouts Timeout-Config]
        [#:max-attempts Exact-Positive-Integer]
        [#:max-redirects Exact-Nonnegative-Integer]
        [#:user-agent (U Bytes String)]
        -> Response)])

(provide
 fandom-get
 fandom-get-api
 timeouts)

(define timeouts (make-timeout-config #:lease 5 #:connect 5))

(: no-headers Headers)
(define no-headers '#hasheq())

(: fandom-get (String String [#:headers (Option Headers)] -> Response))
(define (fandom-get wikiname path #:headers [headers #f])
  (define dest-url (string-append "https://www.fandom.com" path))
  (define host (string-append wikiname ".fandom.com"))
  (log-outgoing wikiname path)
  (get dest-url
       #:timeouts timeouts
       #:headers (hash-set (or headers no-headers) 'Host host)))

(: fandom-get-api (String (Listof (Pair String String)) [#:headers (Option Headers)] -> Response))
(define (fandom-get-api wikiname params #:headers [headers #f])
  (fandom-get wikiname
              (string-append "/api.php?" (params->query params))
              #:headers headers))

(: log-outgoing (String String -> Void))
(define (log-outgoing wikiname path)
  (when (config-true? 'log_outgoing)
    (printf "out: ~a ~a~n" wikiname path)))

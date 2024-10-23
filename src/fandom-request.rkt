#lang typed/racket/base
(require racket/format
         racket/string
         "config.rkt"
         "../lib/url-utils.rkt")
(define-type Headers (HashTable Symbol (U Bytes String)))
(require/typed net/http-easy
  [#:opaque Timeout-Config timeout-config?]
  [#:opaque Response response?]
  [#:opaque Session session?]
  [response-status-code (Response -> Natural)]
  [current-session (Parameter Session)]
  [current-user-agent (Parameter (U Bytes String))]
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

(unless (string-contains? (~a (current-user-agent)) "BreezeWiki")
  (current-user-agent
   (format "BreezeWiki/1.0 (~a) ~a"
           (if (config-true? 'canonical_origin)
               (config-get 'canonical_origin)
               "local")
           (current-user-agent))))

(define timeouts (make-timeout-config #:lease 5 #:connect 5))

(: last-failure Flonum)
(define last-failure 0.0)
(: stored-failure (Option Response))
(define stored-failure #f)
(define failure-persist-time 30000)

(: no-headers Headers)
(define no-headers '#hasheq())

(: fandom-get (String String [#:headers (Option Headers)] -> Response))
(define (fandom-get wikiname path #:headers [headers #f])
  (or
   (and ((current-inexact-milliseconds) . < . (+ last-failure failure-persist-time)) stored-failure)
   (let ()
     (define dest-url (string-append "https://www.fandom.com" path))
     (define host (string-append wikiname ".fandom.com"))
     (log-outgoing wikiname path)
     (define res
       (get dest-url
            #:timeouts timeouts
            #:headers (hash-set (or headers no-headers) 'Host host)))
     (when (memq (response-status-code res) '(403 406))
       (set! last-failure (current-inexact-milliseconds))
       (set! stored-failure res))
     res)))

(: fandom-get-api (String (Listof (Pair String String)) [#:headers (Option Headers)] -> Response))
(define (fandom-get-api wikiname params #:headers [headers #f])
  (fandom-get wikiname
              (string-append "/api.php?" (params->query params))
              #:headers headers))

(: log-outgoing (String String -> Void))
(define (log-outgoing wikiname path)
  (when (config-true? 'log_outgoing)
    (printf "out: ~a ~a~n" wikiname path)))

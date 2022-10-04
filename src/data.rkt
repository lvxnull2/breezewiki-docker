#lang racket/base
(require (prefix-in easy: net/http-easy)
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 (struct-out license)
 license-default
 license-auto)

(struct license (text url) #:transparent)
(define license-default (license "CC-BY-SA" "https://www.fandom.com/licensing"))
(define license-hash (make-hash))
(define (license-fetch wikiname)
  (define dest-url
    (format "https://~a.fandom.com/api.php?~a"
            wikiname
            (params->query '(("action" . "query")
                             ("meta" . "siteinfo")
                             ("siprop" . "rightsinfo")
                             ("format" . "json")
                             ("formatversion" . "2")))))
  (log-outgoing dest-url)
  (define res (easy:get dest-url))
  (define data (easy:response-json res))
  (license (jp "/query/rightsinfo/text" data)
           (jp "/query/rightsinfo/url" data)))
(define (license-auto wikiname)
  (if (hash-has-key? license-hash wikiname)
      (hash-ref license-hash wikiname)
      (let ([result (license-fetch wikiname)])
        (hash-set! license-hash wikiname result)
        result)))

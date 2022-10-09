#lang racket/base
(require racket/list
         (prefix-in easy: net/http-easy)
         memo
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 (struct-out siteinfo)
 (struct-out license)
 siteinfo-fetch
 license-default)

(struct siteinfo (sitename basepage license) #:transparent)
(struct license (text url) #:transparent)

(define license-default (license "CC-BY-SA" "https://www.fandom.com/licensing"))

(define/memoize (siteinfo-fetch wikiname) #:hash hash
  (define dest-url
    (format "https://~a.fandom.com/api.php?~a"
            wikiname
            (params->query '(("action" . "query")
                             ("meta" . "siteinfo")
                             ("siprop" . "general|rightsinfo")
                             ("format" . "json")
                             ("formatversion" . "2")))))
  (log-outgoing dest-url)
  (define res (easy:get dest-url))
  (define data (easy:response-json res))
  (siteinfo (jp "/query/general/sitename" data)
            (second (regexp-match #rx"/wiki/(.*)" (jp "/query/general/base" data)))
            (license (jp "/query/rightsinfo/text" data)
                     (jp "/query/rightsinfo/url" data))))

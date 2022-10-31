#lang racket/base
(require racket/list
         racket/match
         (prefix-in easy: net/http-easy)
         memo
         "static-data.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 (struct-out siteinfo^)
 (struct-out license^)
 (struct-out head-data^)
 siteinfo-fetch
 siteinfo-default
 license-default
 head-data-getter
 head-data-default)

(struct siteinfo^ (sitename basepage license) #:transparent)
(struct license^ (text url) #:transparent)
(struct head-data^ (body-class icon-url) #:transparent)

(define license-default (license^ "CC-BY-SA" "https://www.fandom.com/licensing"))
(define siteinfo-default (siteinfo^ "Test Wiki" "Main_Page" license-default))
(define head-data-default (head-data^ "skin-fandomdesktop" (get-static-url "breezewiki-favicon.svg")))

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
  (siteinfo^ (jp "/query/general/sitename" data)
             (second (regexp-match #rx"/wiki/(.*)" (jp "/query/general/base" data)))
             (license^ (jp "/query/rightsinfo/text" data)
                       (jp "/query/rightsinfo/url" data))))

(define/memoize (head-data-getter wikiname) #:hash hash
  ;; data will be stored here, can be referenced by the memoized closure
  (define this-data head-data-default)
  ;; returns the getter
  (λ ([res-in #f])
    (when res-in
      ;; when actual information is provided, parse it into the struct and store it for the future
      (define head-html (jp "/parse/headhtml" res-in ""))
      (define data
        (head-data^
         (match (regexp-match #rx"<body [^>]*class=\"([^\"]*)" head-html)
           [(list _ classes) classes]
           [_ (head-data^-body-class head-data-default)])
         (match (regexp-match #rx"<link rel=\"(?:shortcut )?icon\" href=\"([^\"]*)" head-html)
           [(list _ icon-url) icon-url]
           [_ (head-data^-icon-url head-data-default)])))
      (set! this-data data))
    ;; then no matter what, return the best information we have so far
    this-data))

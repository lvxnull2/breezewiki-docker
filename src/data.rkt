#lang racket/base
(require racket/list
         racket/match
         web-server/http/request-structs
         net/url-string
         (only-in net/cookies/server cookie-header->alist cookie->set-cookie-header make-cookie)
         (prefix-in easy: net/http-easy)
         db
         memo
         "static-data.rkt"
         "whole-utils.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt"
         "../archiver/archiver-database.rkt"
         "config.rkt")

(provide
 (struct-out siteinfo^)
 (struct-out license^)
 (struct-out head-data^)
 (struct-out user-cookies^)
 siteinfo-fetch
 siteinfo-default
 license-default
 head-data-getter
 head-data-default
 user-cookies-getter
 user-cookies-default
 user-cookies-setter
 user-cookies-setter-url)

(struct siteinfo^ (sitename basepage license) #:transparent)
(struct license^ (text url) #:transparent)
(struct head-data^ (body-class icon-url) #:transparent)

(define license-default (license^ "CC-BY-SA" "https://www.fandom.com/licensing"))
(define siteinfo-default (siteinfo^ "Unknown Wiki" "Main_Page" license-default))
(define head-data-default (head-data^ "skin-fandomdesktop" (get-static-url "breezewiki-favicon.svg")))

(when (config-true? 'feature_offline::only)
  (void (get-slc)))

(define/memoize (siteinfo-fetch wikiname) #:hash hash
  (cond
    [(config-true? 'feature_offline::only)
     (when (config-true? 'debug)
       (printf "using offline mode for siteinfo ~a~n" wikiname))
     (define row (query-maybe-row* "select sitename, basepage, license_text, license_url from wiki where wikiname = ?"
                                   wikiname))
     (if row
         (siteinfo^ (vector-ref row 0)
                    (vector-ref row 1)
                    (license^ (vector-ref row 2)
                              (vector-ref row 3)))
         siteinfo-default)]
    [else
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
                          (jp "/query/rightsinfo/url" data)))]))

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

(struct user-cookies^ (theme) #:prefab)
(define user-cookies-default (user-cookies^ 'default))
(define (user-cookies-getter req)
  (define cookie-header (headers-assq* #"cookie" (request-headers/raw req)))
  (define cookies-alist (if cookie-header (cookie-header->alist (header-value cookie-header) bytes->string/utf-8) null))
  (define cookies-hash
    (for/hasheq ([pair cookies-alist])
      (match pair
        [(cons "theme" (and theme (or "light" "dark" "default")))
         (values 'theme (string->symbol theme))]
        [_ (values #f #f)])))
  (user-cookies^
   (hash-ref cookies-hash 'theme (user-cookies^-theme user-cookies-default))))

(define (user-cookies-setter user-cookies)
  (map (λ (c) (header #"Set-Cookie" (cookie->set-cookie-header c)))
       (list (make-cookie "theme" (symbol->string (user-cookies^-theme user-cookies))
                          #:path "/"
                          #:max-age (* 60 60 24 365 10)))))

(define (user-cookies-setter-url req new-settings)
  (format "/set-user-settings?~a"  (params->query `(("next_location" . ,(url->string (request-uri req)))
                                                    ("new_settings" . ,(format "~a" new-settings))))))

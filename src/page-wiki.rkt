#lang racket/base
(require racket/dict
         racket/function
         racket/list
         racket/match
         racket/string
         ; libs
         (prefix-in easy: net/http-easy)
         ; html libs
         html-parsing
         html-writing
         ; web server libs
         net/url
         web-server/http
         web-server/dispatchers/dispatch
         ; my libs
         "application-globals.rkt"
         "config.rkt"
         "data.rkt"
         "pure-utils.rkt"
         "syntax.rkt"
         "tree-updater.rkt"
         "xexpr-utils.rkt"
         "url-utils.rkt")

(provide
 ; used by the web server
 page-wiki
 ; used by page-category, and similar pages that are partially wiki pages
 update-tree-wiki
 preprocess-html-wiki)

(module+ test
  (require rackunit))

(define (preprocess-html-wiki html)
  (define ((rr* find replace) contents)
    (regexp-replace* find contents replace))
  ((compose1
    ; fix navbox list nesting
    ; navbox on right of page has incorrect html "<td ...><li>" and the xexpr parser puts the <li> much further up the tree
    ; add a <ul> to make the parser happy
    ; usage: /fallout/wiki/Fallout:_New_Vegas_achievements_and_trophies
    (rr* #rx"(<td[^>]*>\n?)(<li>)" "\\1<ul>\\2")
    ; change <figcaption><p> to <figcaption><span> to make the parser happy
    (rr* #rx"(<figcaption[^>]*>)[ \t]*<p class=\"caption\">([^<]*)</p>" "\\1<span class=\"caption\">\\2</span>"))
   html))
(module+ test
  (check-equal? (preprocess-html-wiki "<td class=\"va-navbox-column\" style=\"width: 33%\">\n<li>Hey</li>")
                "<td class=\"va-navbox-column\" style=\"width: 33%\">\n<ul><li>Hey</li>")
  (check-equal? (preprocess-html-wiki "<figure class=\"thumb tright\" style=\"width: 150px\"><a class=\"image\"><img></a><noscript><a><img></a></noscript><figcaption class=\"thumbcaption\"> 	<p class=\"caption\">Caption text.</p></figcaption></figure>")
                "<figure class=\"thumb tright\" style=\"width: 150px\"><a class=\"image\"><img></a><noscript><a><img></a></noscript><figcaption class=\"thumbcaption\"><span class=\"caption\">Caption text.</span></figcaption></figure>"))

(define (page-wiki req)
  (define wikiname (path/param-path (first (url-path (request-uri req)))))
  (define user-cookies (user-cookies-getter req))
  (define origin (format "https://~a.fandom.com" wikiname))
  (define path (string-join (map path/param-path (cddr (url-path (request-uri req)))) "/"))
  (define source-url (format "https://~a.fandom.com/wiki/~a" wikiname path))

  (thread-let
   ([dest-res (define dest-url
                (format "~a/api.php?~a"
                        origin
                        (params->query `(("action" . "parse")
                                         ("page" . ,path)
                                         ("prop" . "text|headhtml|langlinks")
                                         ("formatversion" . "2")
                                         ("format" . "json")))))
              (log-outgoing dest-url)
              (easy:get dest-url
                        #:timeouts timeouts
                        #:headers `#hasheq((cookie . ,(format "theme=~a" (user-cookies^-theme user-cookies)))))]
    [siteinfo (siteinfo-fetch wikiname)])

   (cond
     [(eq? 200 (easy:response-status-code dest-res))
      (let* ([data (easy:response-json dest-res)]
             [title (jp "/parse/title" data "")]
             [page-html (jp "/parse/text" data "")]
             [page-html (preprocess-html-wiki page-html)]
             [page (html->xexp page-html)]
             [head-data ((head-data-getter wikiname) data)])
        (if (equal? "missingtitle" (jp "/error/code" data #f))
            (next-dispatcher)
            (response-handler
             (define body
               (generate-wiki-page
                (update-tree-wiki page wikiname)
                #:req req
                #:source-url source-url
                #:wikiname wikiname
                #:title title
                #:head-data head-data
                #:siteinfo siteinfo))
             (define redirect-msg ((query-selector (attribute-selector 'class "redirectMsg") body)))
             (define redirect-query-parameter (dict-ref (url-query (request-uri req)) 'redirect "yes"))
             (define headers
               (build-headers
                always-headers
                ; redirect-query-parameter: only the string "no" is significant:
                ; https://github.com/Wikia/app/blob/fe60579a53f16816d65dad1644363160a63206a6/includes/Wiki.php#L367
                (when (and redirect-msg
                           (not (equal? redirect-query-parameter "no")))
                  (let* ([dest (get-attribute 'href (bits->attributes ((query-selector (λ (t a c) (eq? t 'a)) redirect-msg))))]
                         [value (bytes-append #"0;url=" (string->bytes/utf-8 dest))])
                    (header #"Refresh" value)))))
             (when (config-true? 'debug)
               ; used for its side effects
               ; convert to string with error checking, error will be raised if xexp is invalid
               (xexp->html body))
             (response/output
              #:code 200
              #:headers headers
              (λ (out)
                (write-html body out))))))])))

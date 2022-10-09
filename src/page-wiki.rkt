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
         "xexpr-utils.rkt"
         "url-utils.rkt")

(provide
 ; used by the web server
 page-wiki
 ; used by page-category, and similar pages that are partially wiki pages
 update-tree-wiki
 preprocess-html-wiki)

(module+ test
  (require rackunit)
  (define wiki-document
    '(*TOP*
      (div (@ (class "mw-parser-output"))
           (aside (@ (role "region") (class "portable-infobox pi-theme-wikia pi-layout-default"))
                  (h2 (@ (class "pi-item pi-title") (data-source "title"))
                      "Infobox Title")
                  (figure (@ (class "pi-item pi-image") (data-source "image"))
                          (a (@ (href "https://static.wikia.nocookie.net/nice-image.png") (class "image image-thumbnail") (title ""))
                             (img (@ (src "https://static.wikia.nocookie.net/nice-image-thumbnail.png") (class "pi-image-thumbnail")))))
                  (div (@ (class "pi-item pi-data") (data-source "description"))
                       (h3 (@ (class "pi-data-label"))
                           "Description")
                       (div (@ (class "pi-data-value"))
                            "Mystery infobox!")))
           (div (@ (data-test-collapsesection) (class "collapsible collapsetoggle-inline collapsed"))
                (i (b "This section is hidden for dramatic effect."))
                (div (@ (class "collapsible-content"))
                     (p "Another page link: "
                        (a (@ (data-test-wikilink) (href "https://test.fandom.com/wiki/Another_Page") (title "Another Page"))
                           "Another Page"))))
           (iframe (@ (src "https://example.com/iframe-src")))))))

(define (preprocess-html-wiki html)
  (define (rr* find replace contents)
    (regexp-replace* find contents replace))
  ((compose1
    ; fix navbox list nesting
    ; navbox on right of page has incorrect html "<td ...><li>" and the xexpr parser puts the <li> much further up the tree
    ; add a <ul> to make the parser happy
    ; usage: /fallout/wiki/Fallout:_New_Vegas_achievements_and_trophies
    (curry rr* #rx"(<td[^>]*>\n?)(<li>)" "\\1<ul>\\2")
    ; change <figcaption><p> to <figcaption><span> to make the parser happy
    (curry rr* #rx"(<figcaption[^>]*>)[ \t]*<p class=\"caption\">([^<]*)</p>" "\\1<span class=\"caption\">\\2</span>"))
   html))
(module+ test
  (check-equal? (preprocess-html-wiki "<td class=\"va-navbox-column\" style=\"width: 33%\">\n<li>Hey</li>")
                "<td class=\"va-navbox-column\" style=\"width: 33%\">\n<ul><li>Hey</li>")
  (check-equal? (preprocess-html-wiki "<figure class=\"thumb tright\" style=\"width: 150px\"><a class=\"image\"><img></a><noscript><a><img></a></noscript><figcaption class=\"thumbcaption\"> 	<p class=\"caption\">Caption text.</p></figcaption></figure>")
                "<figure class=\"thumb tright\" style=\"width: 150px\"><a class=\"image\"><img></a><noscript><a><img></a></noscript><figcaption class=\"thumbcaption\"><span class=\"caption\">Caption text.</span></figcaption></figure>"))

(define (update-tree-wiki tree wikiname)
  (update-tree
   (λ (element element-type attributes children)
     ;; replace whole element?
     (cond
       ; wrap tables in a div.table-scroller
       [(and (eq? element-type 'table)
             (has-class? "wikitable" attributes)
             (not (dict-has-key? attributes 'data-scrolling)))
        `(div
          ((class "table-scroller"))
          ((,element-type (@ (data-scrolling) ,@attributes)
                          ,@children)))]
       ; exclude empty figcaptions
       [(and (eq? element-type 'figcaption)
             (or (eq? (length (filter element-is-element? children)) 0)
                 ((query-selector (λ (element-type attributes children)
                                    (eq? element-type 'use))
                                  element))))
        return-no-element]
       ; exclude infobox items that are videos, and gallery items that are videos
       [(and (or (has-class? "pi-item" attributes)
                 (has-class? "wikia-gallery-item" attributes))
             ((query-selector (λ (element-type attributes children)
                                (has-class? "video-thumbnail" attributes))
                              element)))
        return-no-element]
       ; exclude the invisible brackets after headings
       [(and (eq? element-type 'span)
             (has-class? "mw-editsection" attributes))
        return-no-element]
       ; display a link instead of an iframe
       [(eq? element-type 'iframe)
        (define src (car (dict-ref attributes 'src null)))
        `(a
          ((class "iframe-alternative") (href ,src))
          (,(format "Embedded media: ~a" src)))]
       [#t
        (list element-type
              ;; attributes
              ((compose1
                ; uncollapsing
                (curry attribute-maybe-update 'class
                       (λ (class)
                         (string-join
                          ((compose1
                            ; uncollapse all navbox items (bottom of page mass navigation)
                            (curry u
                                   (λ (classlist) (and (eq? element-type 'table)
                                                       (member "navbox" classlist)
                                                       (member "collapsed" classlist)))
                                   (λ (classlist) (filter (curry (negate equal?) "collapsed") classlist)))
                            ; uncollapse portable-infobox sections
                            (curry u
                                   (λ (classlist) (and (eq? element-type 'section)
                                                       (member "pi-collapse" classlist)))
                                   (λ (classlist) (filter (λ (v)
                                                            (and (not (equal? v "pi-collapse-closed"))
                                                                 (not (equal? v "pi-collapse"))))
                                                          classlist)))
                            ; generic: includes article sections and tables, probably more
                            (curry u
                                   (λ (classlist) (and (member "collapsible" classlist)
                                                       (member "collapsed" classlist)))
                                   (λ (classlist) (filter (curry (negate equal?) "collapsed") classlist))))
                           (string-split class " "))
                          " ")))
                ; change links to stay on the same wiki
                (curry attribute-maybe-update 'href
                       (λ (href)
                         ((compose1
                           (λ (href) (regexp-replace #rx"^(/wiki/.*)" href (format "/~a\\1" wikiname)))
                           (λ (href) (regexp-replace (pregexp (format "^https://(~a)\\.fandom\\.com(/wiki/.*)" px-wikiname)) href "/\\1\\2")))
                          href)))
                ; add noreferrer to a.image
                (curry u
                       (λ (v) (and (eq? element-type 'a)
                                   (has-class? "image" v)))
                       (λ (v) (dict-update v 'rel (λ (s)
                                                    (list (string-append (car s) " noreferrer")))
                                           '(""))))
                ; proxy images from inline styles
                (curry attribute-maybe-update 'style
                       (λ (style)
                         (regexp-replace #rx"url\\(['\"]?(.*?)['\"]?\\)" style
                                         (λ (whole url)
                                           (string-append
                                            "url("
                                            (u-proxy-url url)
                                            ")")))))
                ; and also their links, if strict_proxy is set
                (curry u
                       (λ (v)
                         (and (config-true? 'strict_proxy)
                              (eq? element-type 'a)
                              (has-class? "image-thumbnail" v)))
                       (λ (v) (attribute-maybe-update 'href u-proxy-url v)))
                ; proxy images from src attributes
                (curry attribute-maybe-update 'src u-proxy-url)
                ; don't lazyload images
                (curry u
                       (λ (v) (dict-has-key? v 'data-src))
                       (λ (v) (attribute-maybe-update 'src (λ (_) (car (dict-ref v 'data-src))) v)))
                ; don't use srcset - TODO: use srcset?
                (λ (v) (dict-remove v 'srcset)))
               attributes)
              ;; children
              ((compose1
                ; wrap blinking animated images in a slot so they can be animated with CSS
                (curry u
                       (λ (v) (and (has-class? "animated" attributes)
                                   ((length v) . > . 1)))
                       (λ (v)
                         `((span (@ (class "animated-slot__outer") (style ,(format "--steps: ~a" (length v))))
                                 (span (@ (class "animated-slot__inner"))
                                       ,@v))))))
               children))]))
   tree))
(module+ test
  (define transformed
    (parameterize ([(config-parameter 'strict_proxy) "true"])
      (update-tree-wiki wiki-document "test")))
  ; check that wikilinks are changed to be local
  (check-equal? (get-attribute 'href (bits->attributes
                                      ((query-selector
                                        (λ (t a c) (dict-has-key? a 'data-test-wikilink))
                                        transformed))))
                "/test/wiki/Another_Page")
  ; check that a.image has noreferrer
  (check-equal? (get-attribute 'rel (bits->attributes
                                     ((query-selector
                                       (λ (t a c) (and (eq? t 'a)
                                                       (has-class? "image" a)))
                                       transformed))))
                " noreferrer")
  ; check that article collapse sections become uncollapsed
  (check-equal? (get-attribute 'class (bits->attributes
                                       ((query-selector
                                         (λ (t a c) (dict-has-key? a 'data-test-collapsesection))
                                         transformed))))
                "collapsible collapsetoggle-inline")
  ; check that iframes are gone
  (check-false ((query-selector (λ (t a c) (eq? t 'iframe)) transformed)))
  (check-equal? (let* ([alternative ((query-selector (λ (t a c) (has-class? "iframe-alternative" a)) transformed))]
                       [link ((query-selector (λ (t a c) (eq? t 'a)) alternative))])
                  (get-attribute 'href (bits->attributes link)))
                "https://example.com/iframe-src")
  ; check that images are proxied
  (check-equal? (get-attribute 'src (bits->attributes
                                     ((query-selector
                                       (λ (t a c) (eq? t 'img))
                                       transformed))))
                "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fnice-image-thumbnail.png")
  ; check that links to images are proxied
  (check-equal? (get-attribute 'href (bits->attributes
                                      ((query-selector
                                        (λ (t a c) (and (eq? t 'a) (has-class? "image-thumbnail" a)))
                                        transformed))))
                "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fnice-image.png"))

(define (page-wiki req)
  (define wikiname (path/param-path (first (url-path (request-uri req)))))
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
              (easy:get dest-url #:timeouts timeouts)]
    [siteinfo (siteinfo-fetch wikiname)])

   (cond
     [(eq? 200 (easy:response-status-code dest-res))
      (let* ([data (easy:response-json dest-res)]
             [title (jp "/parse/title" data "")]
             [page-html (jp "/parse/text" data "")]
             [page-html (preprocess-html-wiki page-html)]
             [page (html->xexp page-html)]
             [head-html (jp "/parse/headhtml" data "")]
             [body-class (match (regexp-match #rx"<body [^>]*class=\"([^\"]*)" head-html)
                           [(list _ classes) classes]
                           [_ ""])])
        (if (equal? "missingtitle" (jp "/error/code" data #f))
            (next-dispatcher)
            (response-handler
             (define body
               (generate-wiki-page
                (update-tree-wiki page wikiname)
                #:source-url source-url
                #:wikiname wikiname
                #:title title
                #:body-class body-class
                #:siteinfo siteinfo))
             (define redirect-msg ((query-selector (attribute-selector 'class "redirectMsg") body)))
             (define headers (if redirect-msg
                                 (let* ([dest (get-attribute 'href (bits->attributes ((query-selector (λ (t a c) (eq? t 'a)) redirect-msg))))]
                                        [value (bytes-append #"0;url=" (string->bytes/utf-8 dest))])
                                   (list (header #"Refresh" value)))
                                 (list)))
             (when (config-true? 'debug)
               ; used for its side effects
               ; convert to string with error checking, error will be raised if xexp is invalid
               (xexp->html body))
             (response/output
              #:code 200
              #:headers headers
              (λ (out)
                (write-html body out))))))])))

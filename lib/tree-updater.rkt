#lang racket/base
(require racket/dict
         racket/function
         racket/match
         racket/string
         "pure-utils.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 preprocess-html-wiki
 update-tree-wiki)

(define (preprocess-html-wiki html)
  (regexp-replace* #rx"(<(?:td|figcaption)[^>]*?>\n?)(?:<li>|[ \t]*?<p class=\"caption\">(.*?)</p>)"
                   html (λ (whole first-tag [contents #f])
                          (if (eq? (string-ref whole 1) #\f) ;; figcaption
                              (string-append first-tag "<span class=\"caption\">" contents "</span>")
                              (string-append first-tag "<ul><li>")))))

(module+ test
  (check-equal? (preprocess-html-wiki "<td class=\"va-navbox-column\" style=\"width: 33%\">\n<li>Hey</li>")
                "<td class=\"va-navbox-column\" style=\"width: 33%\">\n<ul><li>Hey</li>")
  (check-equal? (preprocess-html-wiki "<figure class=\"thumb tright\" style=\"width: 150px\"><a class=\"image\"><img></a><noscript><a><img></a></noscript><figcaption class=\"thumbcaption\"> 	<p class=\"caption\">Caption text.</p></figcaption></figure>")
                "<figure class=\"thumb tright\" style=\"width: 150px\"><a class=\"image\"><img></a><noscript><a><img></a></noscript><figcaption class=\"thumbcaption\"><span class=\"caption\">Caption text.</span></figcaption></figure>"))

(module+ test
  (require rackunit
           "html-parsing/main.rkt")
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
           (figure (@ (class "thumb tnone"))
                   (a (@ (href "https://static.wikia.nocookie.net/nice-image.png") (class "image") (data-test-figure-a))
                      (img (@ (src "data:image/gif;base64,R0lGODlhAQABAIABAAAAAP///yH5BAEAAAEALAAAAAABAAEAQAICTAEAOw%3D%3D")
                              (data-src "https://static.wikia.nocookie.net/nice-image-thumbnail.png")
                              (class "thumbimage lazyload"))))
                   (noscript
                    (a (@ (href "https://static.wikia.nocookie.net/nice-image.png") (class "image"))
                       (img (@ (src "https://static.wikia.nocookie.net/nice-image-thumbnail.png")
                               (data-src "https://static.wikia.nocookie.net/nice-image-thumbnail.png")
                               (class "thumbimage")))))
                   (figcaption "Test figure!"))
           (iframe (@ (src "https://example.com/iframe-src")))
           (div (@ (class "reviews"))
                (header "GameSpot Expert Reviews"))
           (div (@ (data-test-ampersand) (class "mw-collapsible-content"))
                (& ndash))))))

(define (updater wikiname #:strict-proxy? [strict-proxy? #f])
  ;; precompute wikiurl regex for efficency
  (define wikiurl-regex (pregexp (format "^https://(~a)\\.fandom\\.com(/wiki/.*)$" px-wikiname)))
  ;; precompute link replacement string for efficiency
  (define wiki-substitution (format "/~a\\1" wikiname))

  (define classlist-updater
    (compose1
     ; uncollapse all navbox items (bottom of page mass navigation)
     (curry u
            (λ (classlist) (and ; removed due to scoping, would improve peformance (eq? element-type 'table)
                            (member "navbox" classlist)
                            (member "collapsed" classlist)))
            (λ (classlist) (filter (curry (negate equal?) "collapsed") classlist)))
     ; uncollapse portable-infobox sections
     (curry u
            (λ (classlist) (and ; removed due to scoping, would improve performance (eq? element-type 'section)
                            (member "pi-collapse" classlist)))
            (λ (classlist) (filter (λ (v)
                                     (and (not (equal? v "pi-collapse-closed"))
                                          (not (equal? v "pi-collapse"))))
                                   classlist)))
     ; generic: includes article sections and tables, probably more
     (curry u
            (λ (classlist) (and (member "collapsible" classlist)
                                (member "collapsed" classlist)))
            (λ (classlist) (filter (curry (negate equal?) "collapsed") classlist)))))

  (define ((string-replace-curried from to) str)
    (string-replace str from to))

  (define class-updater
    (compose1
     (string-replace-curried " collapsed" "")
     (string-replace-curried "pi-collapse-closed" "")
     (string-replace-curried "pi-collapse" "")))

  (define (cardimage-class-updater c)
    (string-append c " bw-updated-cardtable-cardimage"))

  (define attributes-updater
    (compose1
     ; uncollapsing
     #;(curry attribute-maybe-update 'class
              (λ (class) (string-join (classlist-updater (string-split class " ")) " ")))
     (curry attribute-maybe-update 'class class-updater)
     ; change links to stay on the same wiki
     (curry attribute-maybe-update 'href
            (λ (href)
              ((compose1
                (λ (href) (regexp-replace #rx"^(/wiki/.*)$" href wiki-substitution))
                (λ (href) (regexp-replace wikiurl-regex href "/\\1\\2")))
               href)))
     ; add noreferrer to a.image
     (curry u
            (λ (v) (and #;(eq? element-type 'a)
                        (has-class? "image" v)))
            (λ (v) (dict-update v 'rel (λ (s)
                                         (list (string-append (car s) " noreferrer")))
                                '(""))))
     ; proxy images from inline styles, if strict_proxy is set
     (curry u
            (λ (v) strict-proxy?)
            (λ (v) (attribute-maybe-update
                    'style
                    (λ (style)
                      (regexp-replace #rx"url\\(['\"]?(.*?)['\"]?\\)" style
                                      (λ (whole url)
                                        (string-append
                                         "url("
                                         (u-proxy-url url)
                                         ")")))) v)))
     ; and also their links, if strict_proxy is set
     (curry u
            (λ (v)
              (and strict-proxy?
                   #;(eq? element-type 'a)
                   (or (has-class? "image-thumbnail" v)
                       (has-class? "image" v))))
            (λ (v) (attribute-maybe-update 'href u-proxy-url v)))
     ; proxy images from src attributes, if strict_proxy is set
     (curry u
            (λ (v) strict-proxy?)
            (λ (v) (attribute-maybe-update 'src u-proxy-url v)))
     ; don't lazyload images
     (curry u
            (λ (v) (dict-has-key? v 'data-src))
            (λ (v) (attribute-maybe-update 'src (λ (_) (car (dict-ref v 'data-src))) v)))
     ; don't use srcset - TODO: use srcset?
     (λ (v) (dict-remove v 'srcset))))

  (define (children-updater attributes children)
    ; more uncollapsing - sample: bandori/wiki/BanG_Dream!_Wikia
    ((λ (children)
       (u
        (λ (v) (has-class? "mw-collapsible-content" attributes))
        (λ (v) (for/list ([element v])
                 (u (λ (element) (element-is-element? element))
                    (λ (element)
                      `(,(car element)
                        (@ ,@(attribute-maybe-update 'style (λ (a) (regexp-replace #rx"display: *none" a "display:inline")) (bits->attributes element)))
                        ,@(filter element-is-content? (cdr element))))
                    element)))
        children))
     ; wrap blinking animated images in a slot so they can be animated with CSS
     ((λ (children)
        (u
         (λ (v) (and (has-class? "animated" attributes)
                     ((length v) . > . 1)))
         (λ (v)
           `((span (@ (class "animated-slot__outer") (style ,(format "--steps: ~a" (length v))))
                   (span (@ (class "animated-slot__inner"))
                         ,@v))))
         children))
      children)))

  (define (updater element element-type attributes children)
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
      ; HACK for /yugioh/wiki/Pot_of_Greed: move card images above tables
      [(and (eq? element-type 'table)
            (has-class? "cardtable" attributes)
            (not (has-class? "bw-updated-cardtable-cardimage" attributes)))
       (define (is-cardimage? t a c) (and (eq? t 'td)
                                          (has-class? "cardtable-cardimage" a)))
       (define cardimage ((query-selector is-cardimage? element)))
       (if (not cardimage)
           (list element-type attributes children)
           (let ([new-cardtable (update-tree
                                 (λ (e t a c)
                                   (if (is-cardimage? t a c)
                                       return-no-element
                                       (list t a c)))
                                 `(,element-type
                                   (@ ,(attribute-maybe-update 'class cardimage-class-updater attributes))
                                   ,@children))])
             (list 'div null (list cardimage new-cardtable))))]
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
      ; remove noscript versions of images because they are likely lower quality than the script versions
      [(and (eq? element-type 'noscript)
            (match children
              ; either the noscript has a.image as a first child...
              [(list (list 'a (list '@ a-att ...) _)) (has-class? "image" a-att)]
              ; or the noscript has img as a first child
              [(list (list 'img _)) #t]
              [_ #f]))
       return-no-element]
      ; remove gamespot reviews/ads
      [(has-class? "reviews" attributes)
       return-no-element]
      [#t
       (list element-type
             ;; attributes
             (attributes-updater #; element-type attributes)
             ;; children
             (children-updater attributes children))]))

  updater)

(define (update-tree-wiki tree wikiname #:strict-proxy? [strict-proxy? #f])
  (update-tree (updater wikiname #:strict-proxy? strict-proxy?) tree))

(module+ test
  (define transformed
    (update-tree-wiki wiki-document "test" #:strict-proxy? #t))
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
                "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fnice-image.png")
  (check-equal? (get-attribute 'href (bits->attributes
                                      ((query-selector
                                        (λ (t a c) (member '(data-test-figure-a) a))
                                        transformed))))
                "/proxy?dest=https%3A%2F%2Fstatic.wikia.nocookie.net%2Fnice-image.png")
  ; check that noscript images are removed
  (check-equal? ((query-selector (λ (t a c) (eq? t 'noscript)) transformed)) #f)
  ; check that gamespot reviews/ads are removed
  (check-equal? ((query-selector (λ (t a c) (has-class? "reviews" a)) transformed)) #f)
  ; check that (& x) sequences are not broken
  (check-equal? ((query-selector (λ (t a c) (dict-has-key? a 'data-test-ampersand)) transformed))
                '(div (@ (data-test-ampersand) (class "mw-collapsible-content"))
                      (& ndash)))
  ; benchmark
  (when (file-exists? "../storage/Frog.html")
    (with-input-from-file "../storage/Frog.html"
      (λ ()
        (define tree (html->xexp (current-input-port)))
        (time (length (update-tree-wiki tree "minecraft")))))))

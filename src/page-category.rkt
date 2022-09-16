#lang racket/base
(require racket/dict
         racket/list
         racket/match
         racket/string
         (prefix-in easy: net/http-easy)
         ; html libs
         html-parsing
         html-writing
         ; web server libs
         net/url
         web-server/http
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         #;(only-in web-server/http/redirect redirect-to)
         "application-globals.rkt"
         "config.rkt"
         "data.rkt"
         "page-wiki.rkt"
         "syntax.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 page-category)

(module+ test
  (require rackunit)
  (define category-json-data
    '#hasheq((batchcomplete . #t) (continue . #hasheq((cmcontinue . "page|4150504c45|41473") (continue . "-||"))) (query . #hasheq((categorymembers . (#hasheq((ns . 0) (pageid . 25049) (title . "Item (entity)")) #hasheq((ns . 0) (pageid . 128911) (title . "3D")) #hasheq((ns . 0) (pageid . 124018) (title . "A Very Fine Item")) #hasheq((ns . 0) (pageid . 142208) (title . "Amethyst Shard")) #hasheq((ns . 0) (pageid . 121612) (title . "Ankle Monitor")))))))))

(define (generate-results-page
         #:source-url source-url
         #:wikiname wikiname
         #:prefixed-category prefixed-category
         #:members-data members-data
         #:page page
         #:body-class [body-class #f]
         #:license [license #f])
  (define members (jp "/query/categorymembers" members-data))
  (generate-wiki-page
   #:source-url source-url
   #:wikiname wikiname
   #:title prefixed-category
   #:body-class body-class
   #:license license
   `(div
     ,(update-tree-wiki page wikiname)
     (hr)
     (h2 ,(format "All Pages in ~a" prefixed-category))
     (div (@ (class "mw-parser-output"))
          (ul (@ (class "my-category-list"))
              ,@(map
                 (λ (result)
                   (define title (jp "/title" result))
                   (define page-path (regexp-replace* #rx" " title "_"))
                   `(li
                     (a (@ (href ,(format "/~a/wiki/~a" wikiname page-path)))
                        ,title)))
                 members))))))

(define (page-category req)
  (response-handler
   (define wikiname (path/param-path (first (url-path (request-uri req)))))
   (define prefixed-category (path/param-path (caddr (url-path (request-uri req)))))
   (define origin (format "https://~a.fandom.com" wikiname))
   (define source-url (format "~a/wiki/~a" origin prefixed-category))

   (thread-let
    ([members-data (define dest-url
                     (format "~a/api.php?~a"
                             origin
                             (params->query `(("action" . "query")
                                              ("list" . "categorymembers")
                                              ("cmtitle" . ,prefixed-category)
                                              ("cmlimit" . "max")
                                              ("formatversion" . "2")
                                              ("format" . "json")))))
                   (printf "out: ~a~n" dest-url)
                   (define dest-res (easy:get dest-url #:timeouts timeouts))
                   (easy:response-json dest-res)]
     [page-data (define dest-url
                  (format "~a/api.php?~a"
                          origin
                          (params->query `(("action" . "parse")
                                           ("page" . ,prefixed-category)
                                           ("prop" . "text|headhtml|langlinks")
                                           ("formatversion" . "2")
                                           ("format" . "json")))))
                (printf "out: ~a~n" dest-url)
                (define dest-res (easy:get dest-url #:timeouts timeouts))
                (easy:response-json dest-res)]
     [license (license-auto wikiname)])

    (define page-html (preprocess-html-wiki (jp "/parse/text" page-data "")))
    (define page (html->xexp page-html))
    (define head-html (jp "/parse/headhtml" page-data ""))
    (define body-class (match (regexp-match #rx"<body [^>]*class=\"([^\"]*)" head-html)
                         [(list _ classes) classes]
                         [_ ""]))
    (define body (generate-results-page
                  #:source-url source-url
                  #:wikiname wikiname
                  #:prefixed-category prefixed-category
                  #:members-data members-data
                  #:page page
                  #:body-class body-class
                  #:license license))

    (when (config-true? 'debug)
      ; used for its side effects
      ; convert to string with error checking, error will be raised if xexp is invalid
      (xexp->html body))
    (response/output
     #:code 200
     (λ (out)
       (write-html body out))))))
(module+ test
  (check-not-false ((query-selector (attribute-selector 'href "/test/wiki/Ankle_Monitor")
                                    (generate-results-page
                                     #:source-url ""
                                     #:wikiname "test"
                                     #:prefixed-category "Category:Items"
                                     #:members-data category-json-data
                                     #:page '(div "page text"))))))

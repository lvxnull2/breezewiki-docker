#lang racket/base
(require racket/dict
         racket/list
         racket/string
         (prefix-in easy: net/http-easy)
         ; html libs
         html-writing
         ; web server libs
         net/url
         web-server/http
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         #;(only-in web-server/http/redirect redirect-to)
         "config.rkt"
         "application-globals.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 page-category)

(module+ test
  (require rackunit)
  (define category-json-data
    '#hasheq((batchcomplete . #t) (continue . #hasheq((cmcontinue . "page|4150504c45|41473") (continue . "-||"))) (query . #hasheq((categorymembers . (#hasheq((ns . 0) (pageid . 25049) (title . "Item (entity)")) #hasheq((ns . 0) (pageid . 128911) (title . "3D")) #hasheq((ns . 0) (pageid . 124018) (title . "A Very Fine Item")) #hasheq((ns . 0) (pageid . 142208) (title . "Amethyst Shard")) #hasheq((ns . 0) (pageid . 121612) (title . "Ankle Monitor")))))))))

(define (generate-results-page dest-url wikiname prefixed-category data)
  (define members (jp "/query/categorymembers" data))
  (generate-wiki-page
   dest-url
   wikiname
   prefixed-category
   `(div (@ (class "mw-parser-output"))
         (ul (@ (class "my-category-list"))
             ,@(map
                (λ (result)
                  (define title (jp "/title" result))
                  (define page-path (regexp-replace* #rx" " title "_"))
                  `(li
                    (a (@ (href ,(format "/~a/wiki/~a" wikiname page-path)))
                       ,title)))
                members)))))

(define (page-category req)
  (response-handler
   (define wikiname (path/param-path (first (url-path (request-uri req)))))
   (define prefixed-category (path/param-path (caddr (url-path (request-uri req)))))

   (define origin (format "https://~a.fandom.com" wikiname))
   (define dest-url (format "~a/api.php?~a"
                            origin
                            (params->query `(("action" . "query")
                                             ("list" . "categorymembers")
                                             ("cmtitle" . ,prefixed-category)
                                             ("cmlimit" . "max")
                                             ("formatversion" . "2")
                                             ("format" . "json")))))
   (printf "out: ~a~n" dest-url)
   (define dest-res (easy:get dest-url #:timeouts timeouts))

   (define data (easy:response-json dest-res))
   (define body (generate-results-page dest-url wikiname prefixed-category data))
   (when (config-true? 'debug)
     ; used for its side effects
     ; convert to string with error checking, error will be raised if xexp is invalid
     (xexp->html body))
   (response/output
    #:code 200
    (λ (out)
      (write-html body out)))))
(module+ test
  (check-not-false ((query-selector (attribute-selector 'href "/test/wiki/Ankle_Monitor")
                                    (generate-results-page "" "test" "Category:Items" category-json-data)))))

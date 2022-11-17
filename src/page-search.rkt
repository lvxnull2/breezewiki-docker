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
         "application-globals.rkt"
         "config.rkt"
         "data.rkt"
         "syntax.rkt"
         "url-utils.rkt"
         "xexpr-utils.rkt")

(provide
 page-search)

(module+ test
  (require rackunit)
  (define search-json-data
    '#hasheq((batchcomplete . #t) (query . #hasheq((search . (#hasheq((ns . 0) (pageid . 219) (size . 1482) (snippet . "") (timestamp . "2022-08-21T08:54:23Z") (title . "Gacha Capsule") (wordcount . 214)) #hasheq((ns . 0) (pageid . 201) (size . 1198) (snippet . "") (timestamp . "2022-07-11T17:52:47Z") (title . "Badges") (wordcount . 181)))))))))

(define (generate-results-page dest-url wikiname query data #:siteinfo [siteinfo #f])
  (define search-results (jp "/query/search" data))
  (generate-wiki-page
   #:source-url dest-url
   #:wikiname wikiname
   #:title query
   #:siteinfo siteinfo
   `(div (@ (class "mw-parser-output"))
         (p ,(format "~a results found for " (length search-results))
            (strong ,query))
         (ul ,@(map
                (λ (result)
                  (let* ([title (jp "/title" result)]
                         [page-path (page-title->path title)]
                         [timestamp (jp "/timestamp" result)]
                         [wordcount (jp "/wordcount" result)]
                         [size (jp "/size" result)])
                    `(li (@ (class "my-result"))
                         (a (@ (class "my-result__link") (href ,(format "/~a/wiki/~a" wikiname page-path)))
                            ,title)
                         (div (@ (class "my-result__info"))
                              "last edited "
                              (time (@ (datetime ,timestamp)) ,(list-ref (string-split timestamp "T") 0))
                              ,(format ", ~a words, ~a kb"
                                       wordcount
                                       (exact->inexact (/ (round (/ size 100)) 10)))))))
                search-results)))))

(define (page-search req)
  (response-handler
   (define wikiname (path/param-path (first (url-path (request-uri req)))))
   (define query (dict-ref (url-query (request-uri req)) 'q #f))
   (define origin (format "https://~a.fandom.com" wikiname))
   (define dest-url
     (format "~a/api.php?~a"
             origin
             (params->query `(("action" . "query")
                              ("list" . "search")
                              ("srsearch" . ,query)
                              ("formatversion" . "2")
                              ("format" . "json")))))

   (thread-let
    ([dest-res (log-outgoing dest-url)
               (easy:get dest-url #:timeouts timeouts)]
     [siteinfo (siteinfo-fetch wikiname)])

    (define data (easy:response-json dest-res))

    (define body (generate-results-page dest-url wikiname query data #:siteinfo siteinfo))
    (when (config-true? 'debug)
      ; used for its side effects
      ; convert to string with error checking, error will be raised if xexp is invalid
      (xexp->html body))
    (response/output
     #:code 200
     #:headers (build-headers always-headers)
     (λ (out)
       (write-html body out))))))
(module+ test
  (check-not-false ((query-selector (attribute-selector 'href "/test/wiki/Gacha_Capsule")
                                    (generate-results-page "" "test" "Gacha" search-json-data)))))

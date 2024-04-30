#lang racket/base
(require racket/string
         (prefix-in easy: net/http-easy)
         "application-globals.rkt"
         "config.rkt"
         "fandom-request.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 search-fandom)

(module+ test
  (require rackunit
           "test-utils.rkt")
  (define search-results-data
    '(#hasheq((ns . 0) (pageid . 219) (size . 1482) (snippet . "") (timestamp . "2022-08-21T08:54:23Z") (title . "Gacha Capsule") (wordcount . 214)) #hasheq((ns . 0) (pageid . 201) (size . 1198) (snippet . "") (timestamp . "2022-07-11T17:52:47Z") (title . "Badges") (wordcount . 181)))))

(define (search-fandom wikiname query params)
  (define res
    (fandom-get-api
     wikiname
     `(("action" . "query")
       ("list" . "search")
       ("srsearch" . ,query)
       ("formatversion" . "2")
       ("format" . "json"))))
  (define json (easy:response-json res))
  (define search-results (jp "/query/search" json))
  (generate-results-content-fandom wikiname query search-results))

;;; generate content for display in the wiki page layout
(define (generate-results-content-fandom wikiname query search-results)
  `(div (@ (class "mw-parser-output"))
        ;; header before the search results showing how many we found
        (p ,(format "~a results found for " (length search-results))
           (strong ,query))
        ;; *u*nordered *l*ist of matching search results
        (ul ,@(for/list ([result search-results])
                (let* ([title (jp "/title" result)]
                       [page-path (page-title->path title)]
                       [timestamp (jp "/timestamp" result)]
                       [wordcount (jp "/wordcount" result)]
                       [size (jp "/size" result)])
                  ;; and make this x-expression...
                  `(li (@ (class "my-result"))
                       (a (@ (class "my-result__link") (href ,(format "/~a/wiki/~a" wikiname page-path))) ; using unquote to insert the result page URL
                          ,title) ; using unquote to insert the result page title
                       (div (@ (class "my-result__info")) ; constructing the line under the search result
                            "last edited "
                            (time (@ (datetime ,timestamp)) ,(list-ref (string-split timestamp "T") 0))
                            ,(format ", ~a words, ~a kb"
                                     wordcount
                                     (exact->inexact (/ (round (/ size 100)) 10))))))))))

(module+ test
  (parameterize ([(config-parameter 'feature_offline::only) "false"])
    (check-not-false ((query-selector (attribute-selector 'href "/test/wiki/Gacha_Capsule")
                                      (generate-results-content-fandom "test" "Gacha" search-results-data))))))

#lang racket/base
(require racket/dict
         racket/string
         (prefix-in easy: net/http-easy)
         "application-globals.rkt"
         "../lib/html-parsing/main.rkt"
         "../lib/url-utils.rkt"
         "whole-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 generate-results-content-solr)

(struct result^ (hl-title hl-body kb words page-path) #:transparent)

(define (generate-results-content-solr wikiname query params)
  ;; grab things from params that would modify the search
  (define op (if (equal? (dict-ref params 'op #f) "or") '("or" . "OR") '("and" . "AND")))
  (define sort (if (equal? (dict-ref params 'sort #f) "len") '("len" . "len desc") '("relevance" . "score desc")))

  ;; the dest-URL will look something like http://localhost:8983/solr/bloons/select?defType=edismax&fl=id%2Clen&hl.defaultSummary=true&hl.encoder=html&hl.fl=title%2Cbody&hl.method=unified&hl.tag.post=%3C%2Fmark%3E&hl.tag.pre=%3Cmark%3E&hl=true&indent=true&q.op=AND&q=blo&qf=title_prefix%20title%5E2.0%20body%20table%5E0.3&useParams=
  (define dest-url
    (format "http://localhost:8983/solr/~a/select?~a"
            wikiname
            (params->query `(("defType" . "edismax")
                             ("q" . ,query)
                             ("q.op" . ,(cdr op))
                             ("qf" . "title_prefix title^2.0 body table^0.3")
                             ("hl" . "true")
                             ("hl.method" . "unified")
                             ("hl.defaultSummary" . "true")
                             ("hl.fl" . "title,body")
                             ("fl" . "id,len,title")
                             ("hl.encoder" . "html")
                             ("hl.tag.pre" . "<mark>")
                             ("hl.tag.post" . "</mark>")
                             ("sort" . ,(cdr sort))))))
  ;; HTTP request to dest-url for search results
  (log-outgoing dest-url)
  (define res (easy:get dest-url #:timeouts timeouts))
  (define json (easy:response-json res))

  ;; build result objects
  (define highlighting (jp "/highlighting" json))
  (define results
    (for/list ([doc (jp "/response/docs" json)])
      (define id (jp "/id" doc))
      (define len (jp "/len" doc))
      (define title (jp "/title" doc))
      (define page-path (page-title->path title))
      (define kb (exact->inexact (/ (round (/ len 100)) 10))) ; divide by 1000 and round to nearest 0.1
      (define words (* (round (/ len 60)) 10)) ; divide by 6 and round to nearest 10
      (define hl (hash-ref highlighting (string->symbol id)))
      (define hl-title (cdr (html->xexp (jp "/title/0" hl))))
      (define hl-body (cdr (html->xexp (string-trim (jp "/body/0" hl)))))
      (result^ hl-title hl-body kb words page-path)))

  (define qtime (exact->inexact (/ (round (/ (jp "/responseHeader/QTime" json) 10)) 100)))

  (define (value-selected? value current-value)
    (append
     `((value ,value))
     (if (equal? value current-value)
         `((selected))
         `())))

  ;; generate content for display in the wiki page layout
  `(div (@ (class "mw-parser-output"))
        (form (@ (class "my-result__filter"))
              (input (@ (type "hidden") (name "q") (value ,query)))
              (select (@ (name "op"))
                      (option (@ ,@(value-selected? "and" (car op))) "All words must match")
                      (option (@ ,@(value-selected? "or" (car op))) "Some words must match"))
              (select (@ (name "sort"))
                      (option (@ ,@(value-selected? "relevance" (car sort))) "Relevant articles")
                      (option (@ ,@(value-selected? "len" (car sort))) "Wordiest articles"))
              (button "Filter results"))
        ;; header before the search results showing how many we found
        (p ,(format "~a results (~a seconds) found for " (jp "/response/numFound" json) qtime)
           (strong ,query))
        ;; *u*nordered *l*ist of matching search results
        (ul ,@(for/list ([result results])
                `(li (@ (class "my-result"))
                     (a (@ (class "my-result__link") (href ,(format "/~a/wiki/~a" wikiname (result^-page-path result)))) ; url
                        ,@(result^-hl-title result)) ; title
                     (p (@ (class "my-result__description")) ,@(result^-hl-body result)) ; result preview
                     (div (@ (class "my-result__info")) ; line under the search result
                          ,(format "~a words, ~a kb of readable stuff"
                                   (result^-words result)
                                   (result^-kb result))))))))

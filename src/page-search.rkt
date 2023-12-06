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
         "search-provider-fandom.rkt"
         "search-provider-solr.rkt"
         "../lib/syntax.rkt"
         "../lib/thread-utils.rkt"
         "../lib/url-utils.rkt"
         "whole-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 page-search)

(define search-providers
  (hash "fandom" generate-results-content-fandom
        "solr" generate-results-content-solr))

;; this takes the info we gathered from fandom and makes the big fat x-expression page
(define (generate-results-page req source-url wikiname query results-content #:siteinfo [siteinfo #f])
  ;; this is *another* helper that builds the wiki page UI and lets me put the search results (or whatever else) in the middle
  (generate-wiki-page
   ;; so I provide my helper function with the necessary context...
   #:req req
   #:source-url source-url
   #:wikiname wikiname
   #:title query
   #:siteinfo siteinfo
   ;; and here's the actual results to display in the wiki page layout
   results-content))

;; will be called when the web browser asks to load the page
(define (page-search req)
  ;; this just means, catch any errors and display them in the browser. it's a function somewhere else
  (response-handler
   ;; the URL will look like "/minecraft/wiki/Special:Search?q=Spawner"
   ;; grab the first part to use as the wikiname, in this case, "minecraft"
   (define wikiname (path/param-path (first (url-path (request-uri req)))))
   ;; grab a dict of url search params
   (define params (url-query (request-uri req)))
   ;; grab the part after ?q= which is the search terms
   (define query (dict-ref params 'q #f))
   ;; figure out which search provider we're going to use
   (define search-provider (hash-ref search-providers (config-get 'feature_offline::search)))

   ;; external special:search url to link at the bottom of the page as the upstream source
   (define external-search-url
     (format "https://~a.fandom.com/wiki/Special:Search?~a"
             wikiname
             (params->query `(("query" . ,query)
                              ("search" . "internal")))))

   ;; simultaneously get the search results, as well as information about the wiki as a whole (its license, icon, name)
   (define-values (results-content siteinfo)
     (thread-values
      (λ ()
        (search-provider wikiname query params)) ;; call the search provider (see file "search-provider-fandom.rkt")
      (λ ()
        (siteinfo-fetch wikiname)))) ;; helper function in another file to get information about the wiki

   ;; calling my generate-results-page function with the information so far in order to get a big fat x-expression
   ;; big fat x-expression goes into the body variable
   (define body (generate-results-page req external-search-url wikiname query results-content #:siteinfo siteinfo))
   ;; error checking
   (when (config-true? 'debug)
     ; used for its side effects
     ; convert to string with error checking, error will be raised if xexp is invalid
     (xexp->html body))
   ;; convert body to HTML and send to browser
   (response/output
    #:code 200
    #:headers (build-headers always-headers)
    (λ (out)
      (write-html body out)))))


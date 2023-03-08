#lang racket/base
(require racket/dict
         racket/file
         racket/function
         racket/list
         racket/match
         racket/path
         racket/string
         ; libs
         (prefix-in easy: net/http-easy)
         file/sha1
         file/gunzip
         json
         ; html libs
         "../lib/html-parsing/main.rkt"
         html-writing
         ; web server libs
         net/url
         web-server/http
         web-server/dispatchers/dispatch
         ; my libs
         "application-globals.rkt"
         "config.rkt"
         "data.rkt"
         "log.rkt"
         "page-wiki.rkt"
         "../lib/archive-file-mappings.rkt"
         "../lib/pure-utils.rkt"
         "../lib/syntax.rkt"
         "../lib/tree-updater.rkt"
         "../lib/xexpr-utils.rkt"
         "../lib/url-utils.rkt")

(provide
 ; used by the web server
 page-wiki-offline)

(module+ test
  (require rackunit))

(define path-archive (anytime-path ".." "storage/archive"))

(define (page-wiki-offline req)
  (response-handler
   (define wikiname (path/param-path (first (url-path (request-uri req)))))
   (define segments (map path/param-path (cdr (url-path (request-uri req)))))
   (define basename (url-segments->basename segments))
   (define maybe-hashed-basename (if ((string-length basename) . > . 240)
                                     (sha1 (string->bytes/latin-1 basename))
                                     basename))

   (define user-cookies (user-cookies-getter req))
   (define theme (user-cookies^-theme user-cookies))

   (log-page-request #t wikiname maybe-hashed-basename theme)

   (define archive-format
     (case (config-get 'feature_offline::format)
       [(".json" "json") (cons "~a.json" (λ () (read-json)))]
       [(".json.gz" "json.gz") (cons "~a.json.gz" (λ ()
                                                    (define-values (in out) (make-pipe))
                                                    (gunzip-through-ports (current-input-port) out)
                                                    (read-json in)))]
       [else (error 'archive-format "unknown archive format configured")]))
   (define fs-path (build-path path-archive wikiname (format (car archive-format) maybe-hashed-basename)))
   (define source-url (format "https://~a.fandom.com/wiki/~a" wikiname (basename->name-for-query basename)))
   (cond
     [(not (file-exists? fs-path))
      (unless (config-true? 'feature_offline::only)
        (next-dispatcher))
      (define mirror-path (url->string (request-uri req)))
      (define body
        (generate-wiki-page
         `(div (@ (class "unsaved-page"))
               (style ".unsaved-page a { text-decoration: underline !important }")
               (p "breezewiki.com doesn't have this page saved.")
               (p "You can see this page by visiting a BreezeWiki mirror:")
               (ul
                (li (a (@ (href ,(format "https://antifandom.com~a" mirror-path))) "View on antifandom.com"))
                (li (a (@ (href ,(format "https://bw.artemislena.eu~a" mirror-path))) "View on artemislena.eu"))
                (li (a (@ (href ,source-url)) "or, you can see the original page on Fandom (ugh)")))
               (p "If you'd like " ,wikiname ".fandom.com to be added to breezewiki.com, " (a (@ (href "https://lists.sr.ht/~cadence/breezewiki-requests")) "let me know about it!")))
         #:req req
         #:source-url source-url
         #:wikiname wikiname
         #:title (url-segments->guess-title segments)
         #:online-styles #f
         #:siteinfo (siteinfo-fetch wikiname)
         ))
      (when (config-true? 'debug)
        ; used for its side effects
        ; convert to string with error checking, error will be raised if xexp is invalid
        (xexp->html body))
      (response/output
       #:code 200
       #:headers always-headers
       (λ (out)
         (write-html body out)))]
     [#t
      (when (config-true? 'debug)
        (printf "using offline mode for ~v~n" fs-path))
      (response-handler
       (define data (with-input-from-file fs-path (cdr archive-format)))
       (define article-title (jp "/parse/title" data))
       (define original-page (html->xexp (preprocess-html-wiki (jp "/parse/text" data))))
       (define page ((query-selector (λ (t a c) (has-class? "mw-parser-output" a)) original-page)))
       (define initial-head-data ((head-data-getter wikiname) data))
       (define head-data
         (case theme
           [(light dark)
            (struct-copy head-data^ initial-head-data
                         [body-class (regexp-replace #rx"(theme-fandomdesktop-)(light|dark)"
                                                     (head-data^-body-class initial-head-data)
                                                     (format "\\1~a" theme))])]
           [else initial-head-data]))
       (define body
         (generate-wiki-page
          (update-tree-wiki page wikiname)
          #:req req
          #:source-url source-url
          #:wikiname wikiname
          #:title article-title
          #:online-styles #f
          #:head-data head-data
          #:siteinfo (siteinfo-fetch wikiname)
          ))
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
          (write-html body out))))])))

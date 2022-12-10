#lang racket/base
(require racket/file
         racket/list
         racket/runtime-path
         racket/string
         json
         (prefix-in easy: net/http-easy)
         html-parsing
         html-writing
         web-server/http
         "config.rkt"
         "data.rkt"
         "niwa-data.rkt"
         "static-data.rkt"
         "pure-utils.rkt"
         "xexpr-utils.rkt"
         "url-utils.rkt")

(provide
 ; headers to always send on all http responses
 always-headers
 ; timeout durations for http-easy requests
 timeouts
 ; generates a consistent footer
 application-footer
 ; generates a consistent template for wiki page content to sit in
 generate-wiki-page
 ; generates a minimal but complete redirect to another page
 generate-redirect)

(module+ test
  (require rackunit
           html-writing
           "test-utils.rkt"))

(define always-headers
  (list (header #"Referrer-Policy" #"same-origin") ; header to not send referers to fandom
        (header #"Link" (string->bytes/latin-1 link-header))))
(define timeouts (easy:make-timeout-config #:lease 5 #:connect 5))

(define-runtime-path path-static "../static")
(define theme-icons
  (for/hasheq ([theme '(default light dark)])
    (values theme
            (html->xexp (file->string (build-path path-static (format "icon-theme-~a.svg" theme)) #:mode 'binary)))))

(define (application-footer source-url #:license [license-in #f])
  (define license (or license-in license-default))
  `(footer (@ (class "custom-footer"))
           (div (@ (class ,(if source-url "custom-footer__cols" "internal-footer")))
                (div (p
                      (img (@ (class "my-logo") (src ,(get-static-url "breezewiki.svg")))))
                     (p
                      (a (@ (href "https://gitdab.com/cadence/breezewiki"))
                         ,(format "~a source code" (config-get 'application_name))))
                     (p
                      (a (@ (href "https://docs.breezewiki.com"))
                         "Documentation and more information"))
                     (p
                      (a (@ (href "https://lists.sr.ht/~cadence/breezewiki-discuss"))
                         "Discussions / Bug reports / Feature requests"))
                     ,(if (config-true? 'instance_is_official)
                          `(p ,(format "This instance is run by the ~a developer, " (config-get 'application_name))
                              (a (@ (href "https://cadence.moe/contact"))
                                 "Cadence."))
                          `(p
                            ,(format "This unofficial instance is based off the ~a source code, but is not controlled by the code developer." (config-get 'application_name)))))
                ,(if source-url
                     `(div (p "This page displays proxied content from "
                              (a (@ (href ,source-url) (rel "nofollow noreferrer")) ,source-url)
                              ,(format ". Text content is available under the ~a license, " (license^-text license))
                              (a (@ (href ,(license^-url license)) (rel "nofollow")) "see license info.")
                              " Media files may have different copying restrictions.")
                           (p ,(format "Fandom is a trademark of Fandom, Inc. ~a is not affiliated with Fandom." (config-get 'application_name))))
                     `(div (p "Text content on wikis run by Fandom is available under the Creative Commons Attribution-Share Alike License 3.0 (Unported), "
                              (a (@ (href "https://www.fandom.com/licensing") (rel "nofollow")) "see license info.")
                              " Media files and official Fandom documents have different copying restrictions.")
                           (p ,(format "Fandom is a trademark of Fandom, Inc. ~a is not affiliated with Fandom." (config-get 'application_name))))))))

;; generate a notice with a link if a fandom wiki has a replacement as part of NIWA or similar
;; if the wiki has no replacement, display nothing
(define (niwa-notice wikiname title)
  (define ind (findf (λ (item) (member wikiname (first item))) niwa-data))
  (if ind
      (let* ([search-page (format "/Special:Search?~a"
                                  (params->query `(("search" . ,title)
                                                   ("go" . "Go"))))]
             [go (if (string-suffix? (third ind) "/")
                     (regexp-replace #rx"/$" (third ind) (λ (_) search-page))
                     (let* ([joiner (second (regexp-match #rx"/(w[^./]*)/" (third ind)))])
                       (regexp-replace #rx"/w[^./]*/.*$" (third ind) (λ (_) (format "/~a~a" joiner search-page)))))])
        `(aside (@ (class "niwa__notice"))
                (h1 (@ (class "niwa__header")) ,(second ind) " has its own website separate from Fandom.")
                (a (@ (class "niwa__go") (href ,go)) "Read " ,title " on " ,(second ind) " →")
                (div (@ (class "niwa__cols"))
                     (div (@ (class "niwa__left"))
                          (p "Most major Nintendo wikis are part of the "
                             (a (@ (href "https://www.niwanetwork.org/about/")) "Nintendo Independent Wiki Alliance")
                             " and have their own wikis off Fandom. You can help this wiki by "
                             (a (@ (href ,go)) "visiting it directly."))
                          (p ,(fifth ind))
                          (div (@ (class "niwa__divider")))
                          (p "Why are you seeing this message? Fandom refuses to delete or archive their copy of this wiki, so that means their pages will appear high up in search results. Fandom hopes to get clicks from readers who don't know any better.")
                          (p (@ (class "niwa__feedback")) "This notice brought to you by BreezeWiki / " (a (@ (href "https://www.kotaku.com.au/2022/10/massive-zelda-wiki-reclaims-independence-six-months-before-tears-of-the-kingdom/")) "Info & Context") " / " (a (@ (href "https://docs.breezewiki.com/Reporting_Bugs.html")) "Feedback?")))
                     (div (@ (class "niwa__right"))
                          (img (@ (class "niwa__logo") (src ,(format "https://www.niwanetwork.org~a" (fourth ind)))))))))
      ""))

(define (generate-wiki-page
         content
         #:req req
         #:source-url source-url
         #:wikiname wikiname
         #:title title
         #:head-data [head-data-in #f]
         #:siteinfo [siteinfo-in #f]
         #:user-cookies [user-cookies-in #f])
  (define siteinfo (or siteinfo-in siteinfo-default))
  (define head-data (or head-data-in ((head-data-getter wikiname))))
  (define user-cookies (or user-cookies-in (user-cookies-getter req)))
  (define (required-styles origin)
    (map (λ (dest-path)
           (define url (format dest-path origin))
           (if (config-true? 'strict_proxy)
               (u-proxy-url url)
               url))
         `(#;"~a/load.php?lang=en&modules=skin.fandomdesktop.styles&only=styles&skin=fandomdesktop"
           #;"~a/load.php?lang=en&modules=ext.gadget.dungeonsWiki%2CearthWiki%2Csite-styles%2Csound-styles&only=styles&skin=fandomdesktop"
           #;"~a/load.php?lang=en&modules=site.styles&only=styles&skin=fandomdesktop"
           ; combine the above entries into a single request for potentially extra speed - fandom.com doesn't even do this!
           ,(format "~~a/wikia.php?controller=ThemeApi&method=themeVariables&variant=~a" (user-cookies^-theme user-cookies))
           "~a/load.php?lang=en&modules=skin.fandomdesktop.styles%7Cext.fandom.PortableInfoboxFandomDesktop.css%7Cext.fandom.GlobalComponents.CommunityHeaderBackground.css%7Cext.gadget.site-styles%2Csound-styles%7Csite.styles&only=styles&skin=fandomdesktop")))
  `(*TOP*
    (*DECL* DOCTYPE html)
    (html
     (head
      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
      (title ,(format "~a | ~a+~a"
                      title
                      (regexp-replace #rx" ?Wiki$" (siteinfo^-sitename siteinfo) "")
                      (config-get 'application_name)))
      ,@(map (λ (url)
               `(link (@ (rel "stylesheet") (type "text/css") (href ,url))))
             (required-styles (format "https://~a.fandom.com" wikiname)))
      (link (@ (rel "stylesheet") (type "text/css") (href ,(get-static-url "main.css"))))
      (script "const BWData = "
              ,(jsexpr->string (hasheq 'wikiname wikiname
                                       'strict_proxy (config-true? 'strict_proxy))))
      ,(if (config-true? 'feature_search_suggestions)
           `(script (@ (type "module") (src ,(get-static-url "search-suggestions.js"))))
           "")
      (script (@ (type "module") (src ,(get-static-url "countdown.js"))))
      (link (@ (rel "icon") (href ,(u (λ (v) (config-true? 'strict_proxy))
                                      (λ (v) (u-proxy-url v))
                                      (head-data^-icon-url head-data))))))
     (body (@ (class ,(head-data^-body-class head-data)))
           (div (@ (class "main-container"))
                (div (@ (class "fandom-community-header__background tileHorizontally header")))
                (div (@ (class "page"))
                     (main (@ (class "page__main"))
                           ,(niwa-notice wikiname title)
                           (div (@ (class "custom-top"))
                                (h1 (@ (class "page-title")) ,title)
                                (nav (@ (class "sitesearch"))
                                     (form (@ (action ,(format "/~a/search" wikiname))
                                              (class "bw-search-form")
                                              (id "bw-pr-search-form"))
                                           (label (@ (for "bw-search-input")) "Search ")
                                           (div (@ (id "bw-pr-search-input"))
                                                (input (@ (type "text") (name "q") (id "bw-search-input") (autocomplete "off"))))
                                           (div (@ (class "bw-ss__container") (id "bw-pr-search-suggestions"))))
                                     (div (@ (class "bw-theme__select"))
                                          (span (@ (class "bw-theme__main-label")) "Page theme")
                                          (div (@ (class "bw-theme__items"))
                                           ,@(for/list ([theme '(default light dark)])
                                               (define class
                                                 (if (equal? theme (user-cookies^-theme user-cookies))
                                                     "bw-theme__item bw-theme__item--selected"
                                                     "bw-theme__item"))
                                               `(a (@ (href ,(user-cookies-setter-url
                                                              req
                                                              (struct-copy user-cookies^ user-cookies
                                                                           [theme theme]))) (class ,class))
                                                   (div (@ (class "bw-theme__icon-container"))
                                                    ,(hash-ref theme-icons theme))
                                                   ,(format "~a" theme)))))))
                           (div (@ (id "content") #;(class "page-content"))
                                (div (@ (id "mw-content-text"))
                                     ,content))
                           ,(application-footer source-url #:license (siteinfo^-license siteinfo)))))))))
(module+ test
  (define page
    (parameterize ([(config-parameter 'strict_proxy) "true"])
      (generate-wiki-page
       '(template)
       #:req test-req
       #:source-url ""
       #:title "test"
       #:wikiname "test")))
  ; check the page is a valid xexp
  (check-not-false (xexp->html page))
  ; check the stylesheet is proxied
  (check-true (string-prefix?
               (get-attribute 'href
                              (bits->attributes
                               ((query-selector
                                 (λ (t a c) (eq? t 'link))
                                 page))))
               "/proxy?dest=https%3A%2F%2Ftest.fandom.com")))

(define (generate-redirect dest #:headers [headers-in '()])
  (define dest-bytes (string->bytes/utf-8 dest))
  (response/output
   #:code 302
   #:headers (append (list (header #"Location" dest-bytes)) headers-in)
   (λ (out)
     (write-html
      `(html
        (head
         (title "Redirecting..."))
        (body
         "Redirecting to "
         (a (@ (href ,dest)) ,dest)
         "..."))
      out))))

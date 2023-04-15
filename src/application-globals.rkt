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
         web-server/http/bindings
         "config.rkt"
         "data.rkt"
         "extwiki-data.rkt"
         "extwiki-generic.rkt"
         "static-data.rkt"
         "../lib/syntax.rkt"
         "../lib/pure-utils.rkt"
         "../lib/xexpr-utils.rkt"
         "../lib/url-utils.rkt")

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
                         "Chat / Bug reports / Feature requests"))
                     ,(if (config-member? 'promotions::indie_wiki_buddy "footer")
                          `(p
                            (a (@ (href "https://getindie.wiki/"))
                               "Get Indie Wiki Buddy browser extension - be redirected to BreezeWiki every time!"))
                          "")
                     ,(if (config-true? 'instance_is_official)
                          `(p ,(format "This instance is run by the ~a developer, " (config-get 'application_name))
                              (a (@ (href "https://cadence.moe/contact"))
                                 "Cadence")
                              ". Proudly hosted by "
                              (a (@ (href "http://alphamethyl.barr0w.net"))
                                 "Barrow Network Solutions" (sup "XD"))
                              ".")
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
(define (extwiki-notice wikiname title)
  (define xt (findf (λ (item) (member wikiname (extwiki^-wikinames item))) extwikis))
  (cond/var
   [xt
    (let* ([group (hash-ref extwiki-groups (extwiki^-group xt))]
           [search-page (format "/Special:Search?~a"
                                (params->query `(("search" . ,title)
                                                 ("go" . "Go"))))]
           [go (if (string-suffix? (extwiki^-home xt) "/")
                   (regexp-replace #rx"/$" (extwiki^-home xt) (λ (_) search-page))
                   (let* ([joiner (second (regexp-match #rx"/(w[^./]*)/" (extwiki^-home xt)))])
                     (regexp-replace #rx"/w[^./]*/.*$" (extwiki^-home xt) (λ (_) (format "/~a~a" joiner search-page)))))]
           [props (extwiki-props^ go)])
      (cond
        [(eq? (extwiki^-banner xt) 'default)
         `(aside (@ (class "niwa__notice"))
                 (h1 (@ (class "niwa__header")) ,(extwiki^-name xt) " has its own website separate from Fandom.")
                 (a (@ (class "niwa__go") (href ,go)) "Read " ,title " on " ,(extwiki^-name xt) " →")
                 (div (@ (class "niwa__cols"))
                      (div (@ (class "niwa__left"))
                           (p ,((extwiki-group^-description group) props))
                           (p ,((extwiki^-description xt) props))
                           (p "This wiki's core community has wholly migrated away from Fandom. You should "
                              (a (@ (href ,go)) "go to " ,(extwiki^-name xt) " now!"))
                           (p (@ (class "niwa__feedback"))
                              ,@(add-between
                                 `(,@(for/list ([link (extwiki-group^-links group)])
                                       `(a (@ (href ,(cdr link))) ,(car link)))
                                   "This notice is from BreezeWiki"
                                   (a (@ (href "https://docs.breezewiki.com/Reporting_Bugs.html")) "Feedback?"))
                                 " / ")))
                      (div (@ (class "niwa__right"))
                           (img (@ (class "niwa__logo") (src ,(extwiki^-logo xt)))))))]
        [(eq? (extwiki^-banner xt) 'parallel)
         `(aside (@ (class "niwa__parallel"))
                 (h1 (@ (class "niwa__header-mini"))
                     "See also "
                     (a (@ (href ,go)) ,(extwiki^-name xt)))
                 (p "This topic has multiple communities of editors, some active on the Fandom wiki, others active on " ,(extwiki^-name xt) ".")
                 (p "For thorough research, be sure to check both communities since they may have different information!")
                 (p (@ (class "niwa__feedback"))
                    ,@(add-between
                       `(,@(for/list ([link (extwiki-group^-links group)])
                             `(a (@ (href ,(cdr link))) ,(car link)))
                         "This notice is from BreezeWiki"
                         (a (@ (href "https://docs.breezewiki.com/Reporting_Bugs.html")) "Feedback?"))
                       " / ")))]
        [(eq? (extwiki^-banner xt) 'empty)
         `(aside (@ (class "niwa__notice niwa__notice--alt"))
                 (h1 (@ (class "niwa__header")) "You will be redirected to " ,(extwiki^-name xt) ".")
                 (p (@ (style "position: relative; top: -12px;")) "This independent wiki community has its own site separate from Fandom.")
                 (a (@ (class "niwa__go") (href ,go)) "Take me there! →")

                 (p (@ (class "niwa__feedback") (style "text-align: left"))
                    ,@(add-between
                       `(,@(for/list ([link (extwiki-group^-links group)])
                             `(a (@ (href ,(cdr link))) ,(car link)))
                         "This notice is from BreezeWiki")
                       " / ")))]))]
   (var fetched-callback (get-redirect-content wikiname))
   [fetched-callback
    (fetched-callback title)]
   [#t ""]))

(define (generate-wiki-page
         content
         #:req req
         #:source-url source-url
         #:wikiname wikiname
         #:title title
         #:head-data [head-data-in #f]
         #:siteinfo [siteinfo-in #f]
         #:user-cookies [user-cookies-in #f]
         #:online-styles [online-styles #t])
  (define siteinfo (or siteinfo-in siteinfo-default))
  (define head-data (or head-data-in ((head-data-getter wikiname))))
  (define user-cookies (or user-cookies-in (user-cookies-getter req)))
  (define origin (format "https://~a.fandom.com" wikiname))
  (define required-styles
    (cond
      [online-styles
       (define styles
         (list
          (format "~a/wikia.php?controller=ThemeApi&method=themeVariables&variant=~a" origin (user-cookies^-theme user-cookies))
          (format "~a/load.php?lang=en&modules=site.styles%7Cskin.fandomdesktop.styles%7Cext.fandom.PortableInfoboxFandomDesktop.css%7Cext.fandom.GlobalComponents.CommunityHeaderBackground.css%7Cext.gadget.site-styles%2Csound-styles&only=styles&skin=fandomdesktop" origin)))
       (if (config-true? 'strict_proxy)
           (map u-proxy-url styles)
           styles)]
      [#t
       (list
        (format "/archive/~a/styles/themeVariables-~a.css" wikiname (user-cookies^-theme user-cookies))
        (format "/archive/~a/styles/site.css" wikiname))]))
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
             required-styles)
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
           ,(let ([extension-eligible?
                   (cond/var
                    [(not req) #f]
                    [(not (config-member? 'promotions::indie_wiki_buddy "banner")) #f]
                    (var ua-pair (assq 'user-agent (request-headers req)))
                    [(not ua-pair) #f]
                    (var ua (string-downcase (cdr ua-pair)))
                    ;; everyone pretends to be chrome, so we do it in reverse
                    ;; this excludes common browsers that don't support the extension
                    [#t (and (not (string-contains? ua "edge/"))
                             (not (string-contains? ua "mobile")))])])
              (if extension-eligible?
                  `(div (@ (class "bw-top-banner"))
                        (div (@ (class "bw-top-banner-rainbow"))
                             "Try " (a (@ (href "https://getindie.wiki/") (target "_blank")) "our affiliated browser extension") " - redirect to BreezeWiki automatically!\n"))
                  ""))
           (div (@ (class "main-container"))
                (div (@ (class "fandom-community-header__background tileHorizontally header")))
                (div (@ (class "page"))
                     (main (@ (class "page__main"))
                           ,(extwiki-notice wikiname title)
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
                                                   `(a (@ (rel "nofollow")
                                                          (href ,(user-cookies-setter-url
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

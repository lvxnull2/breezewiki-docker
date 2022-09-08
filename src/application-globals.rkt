#lang racket/base
(require racket/string
         net/http-easy
         "config.rkt"
         "xexpr-utils.rkt"
         "url-utils.rkt")

(provide
 ; timeout durations for http-easy requests
 timeouts
 ; generates a consistent footer
 application-footer
 ; generates a consistent template for wiki page content to sit in
 generate-wiki-page)

(module+ test
  (require rackunit
           html-writing))

(define timeouts (make-timeout-config #:lease 5 #:connect 5))

(define (application-footer source-url)
  `(footer (@ (class "custom-footer"))
           (div (@ (class ,(if source-url "custom-footer__cols" "internal-footer")))
                (div (p
                      (img (@ (class "my-logo") (src "/static/breezewiki.svg"))))
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
                              (a (@ (href ,source-url) (rel "noreferrer")) ,source-url)
                              ". Text content is available under the Creative Commons Attribution-Share Alike License 3.0 (Unported), "
                              (a (@ (href "https://www.fandom.com/licensing")) "see license info.")
                              " Media files may have different copying restrictions.")
                           (p ,(format "Fandom is a trademark of Fandom, Inc. ~a is not affiliated with Fandom." (config-get 'application_name))))
                     `(div (p "Text content on wikis run by Fandom is available under the Creative Commons Attribution-Share Alike License 3.0 (Unported), "
                              (a (@ (href "https://www.fandom.com/licensing")) "see license info.")
                              " Media files and official Fandom documents have different copying restrictions.")
                           (p ,(format "Fandom is a trademark of Fandom, Inc. ~a is not affiliated with Fandom." (config-get 'application_name))))))))

(define (generate-wiki-page source-url wikiname title content)
  (define (required-styles origin)
    (map (λ (dest-path)
           (define url (format dest-path origin))
           (if (config-true? 'strict_proxy)
               (u-proxy-url url)
               url))
         '(#;"~a/load.php?lang=en&modules=skin.fandomdesktop.styles&only=styles&skin=fandomdesktop"
           #;"~a/load.php?lang=en&modules=ext.gadget.dungeonsWiki%2CearthWiki%2Csite-styles%2Csound-styles&only=styles&skin=fandomdesktop"
           #;"~a/load.php?lang=en&modules=site.styles&only=styles&skin=fandomdesktop"
           ; combine the above entries into a single request for potentially extra speed - fandom.com doesn't even do this!
           "~a/load.php?lang=en&modules=skin.fandomdesktop.styles%7Cext.fandom.PortableInfoboxFandomDesktop.css%7Cext.fandom.GlobalComponents.CommunityHeaderBackground.css%7Cext.gadget.site-styles%2Csound-styles%7Csite.styles&only=styles&skin=fandomdesktop"
           "~a/wikia.php?controller=ThemeApi&method=themeVariables")))
  `(html
    (head
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
     (title ,(format "~a | ~a" title (config-get 'application_name)))
     (style ":root { --theme-page-background-color: #dfdfe0 }") ; fallback in case styles don't load fast enough
     ,@(map (λ (url)
              `(link (@ (rel "stylesheet") (type "text/css") (href ,url))))
            (required-styles (format "https://~a.fandom.com" wikiname)))
     (link (@ (rel "stylesheet") (type "text/css") (href "/static/main.css"))))
    (body (@ (class "skin-fandomdesktop theme-fandomdesktop-light"))
          (div (@ (class "main-container"))
               (div (@ (class "fandom-community-header__background tileHorizontally header")))
               (div (@ (class "page"))
                    (main (@ (class "page__main"))
                          (div (@ (class "custom-top"))
                               (h1 (@ (class "page-title")) ,title)
                               (nav (@ (class "sitesearch"))
                                    (form (@ (action ,(format "/~a/search" wikiname)))
                                          (label "Search "
                                                 (input (@ (type "text") (name "q")))))))
                          (div (@ (id "content") #;(class "page-content"))
                               (div (@ (id "mw-content-text"))
                                    ,content))
                          ,(application-footer source-url)))))))
(module+ test
  (define page
    (parameterize ([(config-parameter 'strict_proxy) "true"])
      (generate-wiki-page "" "test" "test" '(template))))
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

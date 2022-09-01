#lang racket/base

(require html-writing
         web-server/http
         "xexpr-utils.rkt"
         "config.rkt")

(provide
 page-home)

(module+ test
  (require rackunit))

(define examples
  '(("crosscode" "CrossCode_Wiki")
    ("minecraft" "Bricks")
    ("undertale" "Hot_Dog...%3F")
    ("tardis" "Eleanor_Blake")
    ("fireemblem" "God-Shattering_Star")
    ("fallout" "Pip-Boy_3000")))

(define content
  `((h2 "BreezeWiki makes wiki pages on Fandom readable")
    (p "It removes ads, videos, and suggested content, leaving you with a clean page that doesn't consume all your data.")
    (p "If you're looking for an \"alternative\" to Fandom for writing pages, you should look elsewhere. BreezeWiki only lets you read existing pages.")
    (p "BreezeWiki can also be called an \"alternative frontend for Fandom\".")
    (h2 "Example pages")
    (ul
     ,@(map (λ (x)
              `(li (a (@ (href ,(apply format "/~a/wiki/~a" x)))
                      ,(apply format "~a: ~a" x))))
            examples))
    (h2 "How to use")
    (p "While browsing any page on Fandom, you can replace \"fandom.com\" in the address bar with \"breezewiki.com\" to see the BreezeWiki version of that page.")
    (p "After that, you can click the links to navigate around the pages.")
    (p "To get back to Fandom, click the link that's at the bottom of the page.")))

(define body
  `(html
    (head
     (meta (@ (name ")viewport") (content "width=device-width, initial-scale=1")))
     (title "About | BreezeWiki")
     (link (@ (rel "stylesheet") (type "text/css") (href "/static/internal.css")))
     (link (@ (rel "stylesheet") (type "text/css") (href "/static/main.css"))))
    (body (@ (class "skin-fandomdesktop theme-fandomdesktop-light internal"))
          (div (@ (class "main-container"))
               (div (@ (class "fandom-community-header__background tileBoth header")))
               (div (@ (class "page"))
                    (main (@ (class "page__main"))
                          (div (@ (class "custom-top"))
                               (h1 (@ (class "page-title"))
                                   "About BreezeWiki"))
                          (div (@ (id "content") #;(class "page-content"))
                               (div (@ (id "mw-content-text"))
                                    ,@content))
                          (footer (@ (class "custom-footer"))
                                  (div (@ (class "internal-footer"))
                                       (img (@ (class "my-logo") (src "/static/breezewiki.svg")))
                                       ,(if (config-get 'instance-is-official)
                                            `(div
                                              (p ,(format "This instance is run by the ~a developer, " (config-get 'application-name))
                                                (a (@ (href "https://cadence.moe/contact"))
                                                   "Cadence."))
                                              (p "Hosting generously provided by "
                                                 (a (@ (href "://alphamethyl.barr0w.net/"))
                                                    "alphamethyl.")))
                                            `(p
                                              ,(format "This unofficial instance is based off the ~a source code, but is not controlled by the code developer." (config-get 'application-name))))
                                       (p "Text content on wikis run by Fandom is available under the Creative Commons Attribution-Share Alike License 3.0 (Unported), "
                                          (a (@ (href "https://www.fandom.com/licensing")) "see license info.")
                                          " Media files and official Fandom documents have different copying restrictions.")
                                       (p ,(format "Fandom is a trademark of Fandom, Inc. ~a is not affiliated with Fandom." (config-get 'application-name)))))))))))
(module+ test
  (check-not-false (xexp->html body)))

(define (page-home req)
  (response/output
   #:code 200
   (λ (out)
     (write-html body out))))


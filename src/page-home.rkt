#lang racket/base

(require net/url
         html-writing
         web-server/http
         "application-globals.rkt"
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
    (p ,(format "While browsing any page on Fandom, you can replace \"fandom.com\" in the address bar with \"~a\" to see the BreezeWiki version of that page."
               (if (config-true? 'canonical_origin)
                   (url-host (string->url (config-get 'canonical_origin)))
                   "breezewiki.com")))
    (p "After that, you can click the links to navigate around the pages.")
    (p "To get back to Fandom, click the link that's at the bottom of the page.")))

(define body
  `(html
    (head
     (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
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
                          ,(application-footer #f)))))))
(module+ test
  (check-not-false (xexp->html body)))

(define (page-home req)
  (response/output
   #:code 200
   (λ (out)
     (write-html body out))))


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
    ("pokemon" "Eevee")
    ("minecraft" "Bricks")
    ("undertale" "Hot_Dog...%3F")
    ("tardis" "Eleanor_Blake")
    ("fireemblem" "God-Shattering_Star")
    ("fallout" "Pip-Boy_3000")))

(define content
  `((h2 "BreezeWiki makes wiki pages on Fandom readable")
    (p "It removes ads, videos, and suggested content, leaving you with a clean page that doesn't slow down your device or use up your data.")
    (p "BreezeWiki can also be called an \"alternative frontend for Fandom\".")
    (p ,(format "To use BreezeWiki, just replace \"fandom.com\" with \"~a\", and you'll instantly be teleported to a better world."
               (if (config-true? 'canonical_origin)
                   (url-host (string->url (config-get 'canonical_origin)))
                   "breezewiki.com")))
    (p "If you'd like to be automatically sent to BreezeWiki every time in the future, "
       (a (@ (href "https://docs.breezewiki.com/Automatic_Redirection.html")) "check out the tutorial in the manual."))
    (h2 "Example pages")
    (ul
     ,@(map (λ (x)
              `(li (a (@ (href ,(apply format "/~a/wiki/~a" x)))
                      ,(apply format "~a: ~a" x))))
            examples))
    (h2 "Testimonials")
    (p (@ (class "testimonial")) ">So glad to never have to touch fandom's garbage platform directly ever again —RNL")
    (p (@ (class "testimonial")) ">you are so right that fandom still sucks even with adblock somehow. even zapping all the stupid padding it still sucks —Minimus")
    (p (@ (class "testimonial")) ">attempting to go to a wiki's forum page with breezewiki doesn't work, which is based honestly —Tom Skeleton")
    (p (@ (class "testimonial")) ">Fandom pages crashing and closing, taking forever to load and locking up as they load the ads on the site... they are causing the site to crash because they are trying to load video ads both at the top and bottom of the site as well as two or three banner ads, then a massive top of site ad and eventually my anti-virus shuts the whole site down because it's literally pulling more resources than WoW in ultra settings... —Anonymous")
    (h2 "What BreezeWiki isn't")
    (p "BreezeWiki isn't an \"alternative\" to Fandom, and it doesn't let you edit or write new pages.")
    (p "If you want to create your own wiki, try Miraheze!")))

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


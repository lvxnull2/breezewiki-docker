#lang racket/base

(require net/url
         html-writing
         web-server/http
         "application-globals.rkt"
         "data.rkt"
         "static-data.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt"
         "config.rkt")

(provide
 page-home)

(module+ test
  (require rackunit))

(define examples
  '(("minecraft" "Bricks")
    ("crosscode" "CrossCode Wiki")
    ("undertale" "Hot Dog...?")
    ("tardis" "Eleanor Blake")
    ("zelda" "Boomerang")))

(define content
  `((h2 "BreezeWiki makes wiki pages on Fandom readable")
    (p "It removes ads, videos, and suggested content, leaving you with a clean page that doesn't slow down your device or use up your data.")
    (p ,(format "To use BreezeWiki, just replace \"fandom.com\" with \"~a\", and you'll instantly be teleported to a better world."
                (if (config-true? 'canonical_origin)
                    (url-host (string->url (config-get 'canonical_origin)))
                    "breezewiki.com")))
    (p "If you'd like to be automatically sent to BreezeWiki every time in the future, "
       ,@(if (config-member? 'promotions::indie_wiki_buddy "home")
             `((a (@ (href "https://getindie.wiki")) "get our affiliated browser extension (NEW!)")
               " or ")
             null)
       (a (@ (href "https://docs.breezewiki.com/Automatic_Redirection.html")) "check out the tutorial in the manual."))
    (p "BreezeWiki is available on several different websites called " (a (@ (href "https://en.wikipedia.org/wiki/Mirror_site")) "mirrors") ". Each is independently run. If one mirror is offline, the others still work. "
       (a (@ (href "https://docs.breezewiki.com/Links.html#%28part._.Mirrors%29")) "See the list."))
    (h2 "Find a page")
    (form (@ (action "/search"))
          (label (@ (class "paired__label"))
                 "Wiki name"
                 (input (@ (name "wikiname") (class "paired__input") (type "text") (placeholder "pokemon") (required))))
          (label (@ (class "paired__label"))
                 "Search query"
                 (input (@ (name "q") (class "paired__input") (type "text") (placeholder "Eevee"))))
          (button "Search"))
    (h2 "Example pages")
    (ul
     ,@(map (λ (x)
              `(li (a (@ (href ,(format "/~a/wiki/~a" (car x) (page-title->path (cadr x)))))
                      ,(apply format "~a: ~a" x))))
            examples))
    (h2 "Testimonials")
    (p (@ (class "testimonial")) ">so glad someone introduced me to a F*ndom alternative (BreezeWiki) because that x-factorized spillway of an ad-infested radioactive dumpsite can go die in a fire —RB")
    (p (@ (class "testimonial")) ">apparently there are thousands of people essentially running our company " (em "for free") " right now, creating tons of content, and we just put ads on top of it and they're not even employees. thousands of people we can't lay off. thousands! —" (a (@ (href "https://hard-drive.net/fandom-ceo-frustrated-its-impossible-to-lay-off-unpaid-users-who-update-wikias-for-fun/?utm_source=breezewiki") (target "_blank")) "Perkins Miller, Fandom CEO"))
    (p (@ (class "testimonial")) ">attempting to go to a wiki's forum page with breezewiki doesn't work, which is based honestly —Tom Skeleton")
    (p (@ (class "testimonial")) ">Fandom pages crashing and closing, taking forever to load and locking up as they load the ads on the site... they are causing the site to crash because they are trying to load video ads both at the top and bottom of the site as well as two or three banner ads, then a massive top of site ad and eventually my anti-virus shuts the whole site down because it's literally pulling more resources than WoW in ultra settings... —Anonymous")
    (p (@ (class "testimonial")) ">reblogs EXTREMELY appreciated I want that twink* (*fandom wiki) obliterated —footlong")

    (h2 "What BreezeWiki isn't")
    (p "BreezeWiki isn't an \"alternative\" to Fandom, and it doesn't let you edit or write new pages.")
    (p "If you want to create your own wiki, try Miraheze!")))

(define body
  `(*TOP*
    (*DECL* DOCTYPE html)
    (html
     (head
      (meta (@ (name "viewport") (content "width=device-width, initial-scale=1")))
      (title "About | BreezeWiki")
      (link (@ (rel "stylesheet") (type "text/css") (href ,(get-static-url "internal.css"))))
      (link (@ (rel "stylesheet") (type "text/css") (href ,(get-static-url "main.css"))))
      (link (@ (rel "icon") (href ,(head-data^-icon-url head-data-default)))))
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
                           ,(application-footer #f))))))))
(module+ test
  (check-not-false (xexp->html body)))

(define (page-home req)
  (response/output
   #:code 200
   #:headers (build-headers always-headers)
   (λ (out)
     (write-html body out))))


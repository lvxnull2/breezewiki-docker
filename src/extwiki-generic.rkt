#lang racket/base
(require racket/list
         racket/match
         racket/string
         memo
         net/http-easy
         html-parsing
         "../lib/pure-utils.rkt"
         "../lib/syntax.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt")

(provide
 get-redirect-content)

(module+ test
  (require rackunit))

;; fandom wikinames * Title * Main Page * Search page override * API endpoint override
(define wikis
  '(((gallowmere) "MediEvil Wiki" "https://medievil.wiki/w/Main_Page" #f #f)
    ((fallout) "Fallout Wiki" "https://fallout.wiki/wiki/Fallout_Wiki" #f "https://fallout.wiki/api.php")
    ((drawntolife) "Wapopedia" "https://drawntolife.wiki/en/Main_Page" #f "https://drawntolife.wiki/w/api.php")
    ))

(define wikis-hash (make-hash))
(for ([w wikis])
  (for ([wikiname (car w)])
    (hash-set! wikis-hash (symbol->string wikiname) w)))
(module+ test
  (check-equal? (cadr (hash-ref wikis-hash "gallowmere"))
                "MediEvil Wiki"))

(define (parse-table table)
  (define rows (query-selector (λ (t a c) (eq? t 'tr)) table))
  (define header-row (rows))
  (define column-names
    (for/list ([th (in-producer (query-selector (λ (t a c) (eq? t 'th)) header-row) #f)])
      (string->symbol (string-downcase (string-trim (findf string? th))))))
  (define data-row (rows))
  (for/hasheq ([col-name column-names]
               [col-value (in-producer (query-selector (λ (t a c) (eq? t 'td)) data-row) #f)])
    (values col-name (filter element-is-content? (cdr col-value)))))
(module+ test
  (check-equal? (parse-table (html->xexp "<table> <tbody><tr> <th>Links</th></tr> <tr> <td><a target=\"_blank\" rel=\"nofollow noreferrer noopener\" class=\"external text\" href=\"https://sirdanielfortesque.proboards.com/\">Forum</a></td></tr></tbody></table>"))
                '#hasheq((links . ((a (@ (target "_blank") (rel "nofollow noreferrer noopener") (class "external text") (href "https://sirdanielfortesque.proboards.com/")) "Forum"))))))

(define (table->links table)
  (define v (hash-ref table 'links #f))
  (cond/var
   [(not v) (values null '("Data table must have a \"Links\" column"))]
   (var links (filter (λ (a) (and (pair? a) (eq? (car a) 'a))) v)) ; <a> elements
   [(null? links) (values null '("Links column must have at least one link"))]
   [#t (values links null)]))

(define (table->logo table)
  (define logo (hash-ref table 'logo #f))
  (cond/var
   [(not logo) (values #f '("Data table must have a \"Logo\" column"))]
   [(null? logo) (values #f '("Logo table column must have a link"))]
   (var href (get-attribute 'href (bits->attributes (car (hash-ref table 'logo)))))
   (var src (get-attribute 'src (bits->attributes (car (hash-ref table 'logo)))))
   (var true-src (or href src))
   [(not true-src) (values #f '("Logo table column must have a link"))]
   [#t (values true-src null)]))

(define (get-api-endpoint wiki)
  (define main-page (third wiki))
  (define override (fifth wiki))
  (or override
      (match main-page
        [(regexp #rx"/$") (string-append main-page "api.php")]
        [(regexp #rx"^(.*)/wiki/" (list _ domain)) (string-append domain "/w/api.php")]
        [(regexp #rx"^(.*)/w/" (list _ domain)) (string-append domain "/api.php")]
        [_ (error 'get-api-endpoint "unknown url format: ~a" main-page)])))

(define (get-search-page wiki)
  (define main-page (third wiki))
  (define override (fourth wiki))
  (or override
      (match main-page
        [(regexp #rx"/$") (string-append main-page "Special:Search")]
        [(regexp #rx"^(.*/(?:en|w[^./]*)/)" (list _ wiki-prefix)) (string-append wiki-prefix "Special:Search")]
        [_ (error 'get-search-page "unknown url format: ~a" main-page)])))

(define/memoize (get-redirect-content wikiname) #:hash hash
  (define wiki (hash-ref wikis-hash wikiname #f))
  (cond
    [wiki
     (define display-name (cadr wiki))
     (define endpoint (string-append (get-api-endpoint wiki) "?action=parse&page=MediaWiki:BreezeWikiRedirect&prop=text&formatversion=2&format=json"))
     (define res (get endpoint))
     (define html (jp "/parse/text" (response-json res)))
     (define content ((query-selector (λ (t a c) (has-class? "mw-parser-output" a))
                                      (html->xexp html))))
     (define body (for/list ([p (in-producer (query-selector (λ (t a c) (eq? t 'p)) content) #f)]) p))
     (define table (parse-table ((query-selector (λ (t a c) (eq? t 'table)) content))))
     (define-values (links links-errors) (table->links table))
     (define-values (logo logo-errors) (table->logo table))
     (define construct-errors (append links-errors logo-errors))
     (λ (title)
       (define go
         (string-append (get-search-page wiki)
                        "?"
                        (params->query `(("search" . ,title)
                                         ("go" . "Go")))))
       `(aside (@ (class "niwa__notice"))
               (h1 (@ (class "niwa__header")) ,display-name " has its own website separate from Fandom.")
               (div (@ (class "niwa__cols"))
                    (div (@ (class "niwa__left"))
                         (a (@ (class "niwa__go") (href ,go)) "Read " ,title " on " ,display-name " →")
                         ,@body
                         (p "This external wiki is a helpful alternative to Fandom. You should "
                            (a (@ (href ,go)) "check it out now!")))
                    ,(if logo
                         `(div (@ (class "niwa__right"))
                               (img (@ (class "niwa__logo") (src ,logo))))
                         ""))
               ,(if (pair? links)
                    `(p (@ (class "niwa__feedback"))
                        ,@(add-between links " / "))
                    "")
               ,(if (pair? construct-errors)
                    `(ul
                      ,@(for/list ([error construct-errors])
                          `(li ,error)))
                    "")))]
    [#t #f]))
(module+ test
  (check-not-false ((get-redirect-content "gallowmere") "MediEvil Wiki")))

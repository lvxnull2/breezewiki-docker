#lang racket/base
(require racket/dict
         racket/function
         racket/generator
         racket/match
         racket/string
         (only-in json-pointer json-pointer-value)
         (only-in web-server/http response/output)
         "pure-utils.rkt")

(provide
 ;; with whole xexprs
 ; xexpr for an "empty" element, which in reality uses <template>
 return-no-element
 ; query a tree for elements matching a condition
 query-selector
 ; update a tree with a function called on each element
 update-tree

 ;; with bits of xexprs
 ; predicates
 element-is-bits?
 element-is-xattributes?
 element-is-element?
 element-is-content?

 ;; with attributes
 ; find the attributes in some bits of an element
 bits->attributes
 ; get attribute value from some attributes
 get-attribute
 ; update an attribute if it is present (otherwise no change)
 attribute-maybe-update
 ; make an attribute selector for use in query-selector
 attribute-selector
 ; do these attributes have a certain value in their class?
 has-class?

 ;; with json
 ; get value in json structure using a *j*son *p*ointer, optionally with default value for not present
 jp

 ; error catching for http responses
 response-handler)

(module+ test
  (require rackunit)
  (define demo-attributes
    '(span (@ (title "Inside joke."))
           "To get to the other side."
           (@ (style "color: blue"))))
  (define demo-document
    '(html
      (@ (lang "en"))
      (head
       (title "Hello world!"))
      (body
       (h1 "Hello world!")
       (p "Welcome to my "
          (span (@ (style "color: yellow")
                   (title "Really."))
                (em "cool"))
          "website.")))))

; replacing with a template element removes it from the rendered document
(define return-no-element '(template
                            ()
                            ()))

; "bits" is attributes or real elements (non-string)
(define (element-is-bits? element)
  (pair? element))
(module+ test
  (check-true (element-is-bits? '(span "hi")))
  (check-true (element-is-bits? '(@ (alt "Cute cat."))))
  (check-false (element-is-bits? "hi")))

; "xattributes" is attributes hugged by @
(define (element-is-xattributes? element)
  (and (element-is-bits? element) (eq? '@ (car element))))
(module+ test
  (check-false (element-is-xattributes? '(span "hi")))
  (check-true (element-is-xattributes? '(@ (alt "Cute cat."))))
  (check-false (element-is-xattributes? '((alt "Cute cat."))))
  (check-false (element-is-xattributes? "hi")))

; "element" is a real element with a type and everything (non-string, non-attributes)
(define (element-is-element? element)
  (and (element-is-bits? element) (not (eq? (car element) '&))(not (element-is-xattributes? element))))
(module+ test
  (check-true (element-is-element? '(span "hi")))
  (check-false (element-is-element? '(@ (alt "Cute cat."))))
  (check-false (element-is-element? "hi"))
  (check-false (element-is-element? '(& ndash))))

; "element content" is a real element or a string or a (& x) sequence
(define (element-is-content? element)
  (or (string? element) (element-is-element? element) (and (pair? element) (eq? (car element) '&))))
(module+ test
  (check-true (element-is-content? '(span "hi")))
  (check-false (element-is-content? '(@ (alt "Cute cat."))))
  (check-true (element-is-content? "hi")))

; get the actual attributes, leaving out the @ signs
(define (xattributes->attributes xattrs)
  (filter pair? xattrs))

(define (bits->attributes bits)
 ; (append) is a clean and general approach to finding and combining any attributes
  (xattributes->attributes (apply append (filter element-is-xattributes? bits))))
(module+ test
  (check-equal? (bits->attributes demo-attributes)
                '((title "Inside joke.") (style "color: blue"))))

(define (get-attribute name attributes)
  (define a (assq name attributes))
  (if (pair? a)
      (cadr a)
      #f))
(module+ test
  (check-equal? (get-attribute 'title (bits->attributes demo-attributes)) "Inside joke."))

(define (attribute-maybe-update key updater attributes)
  (alist-maybe-update attributes key (λ (v) (map updater v))))
(module+ test
  (check-equal? (attribute-maybe-update 'a (λ (x) (+ x 10)) '((a 5) (b 6)))
                '((a 15) (b 6))))

(define (attribute-selector name value)
  (λ (element-type attributes children)
    (equal? (get-attribute name attributes) value)))

(define (query-selector selector element #:include-text? [include-text? #f])
  (generator
   ()
   (let loop ([element element])
     (define element-type (car element))
     (define attributes (bits->attributes (cdr element)))
     (define children (filter element-is-element? (cdr element))) ; only recurse through real children
     (cond
       [(equal? element-type '*DECL*) #f]
       [(equal? element-type '@) #f]
       [#t
        (when (if include-text?
                   (selector element-type attributes children (filter string? (cdr element)))
                   (selector element-type attributes children))
          (yield element))
        (for ([child children]) (loop child))]))
  #f))
(module+ test
  (let ([result (query-selector (attribute-selector 'title "Really.")
                                demo-document)])
    (check-equal? (result) '(span (@ (style "color: yellow")
                                     (title "Really."))
                                  (em "cool")))
    (check-equal? (result) #f)))

(define (update-tree transformer element)
  (let loop ([element element])
    (define element-type (car element))
    (define attributes (bits->attributes (cdr element)))
    (define contents (filter element-is-content? (cdr element))) ; provide elements and strings
    (cond
      [(equal? element-type '*DECL*)
       ; declarations like <!DOCTYPE html> get mapped as attributes as if the element were (*DECL* (@ (DOCTYPE) (html)))
       (match (transformer element element-type (map list (cdr element)) null)
         [(list element-type attributes contents)
          `(*DECL* ,@(map car attributes))]
         [#f ""])]
      [(member element-type '(@ &))
       ; special element, do nothing
       element]
      [#t
       ; regular element, transform it
       (match (transformer element element-type attributes contents)
         [(list element-type attributes contents)
          (append (list element-type)
                  (if (pair? attributes) (list (append '(@) attributes)) (list))
                  (map (λ (content)
                         (if (element-is-element? content) (loop content) content))
                       contents))])])))
(module+ test
  ; check doctype is preserved when present
  (check-equal? (update-tree (λ (e t a c) (list t a c)) '(*TOP* (*DECL* DOCTYPE html) (html (body "Hey"))))
                '(*TOP* (*DECL* DOCTYPE html) (html (body "Hey"))))
  ; check doctype can be removed if desirable
  (check-equal? (update-tree (λ (e t a c) (if (eq? t '*DECL*) #f (list t a c))) '(*TOP* (*DECL* DOCTYPE html) (html (body "Hey"))))
                '(*TOP* "" (html (body "Hey"))))
  ; check (& x) sequences are preserved
  (check-equal? (update-tree (λ (e t a c) (list t a c)) '(body "Hey" (& nbsp) (a (@ (href "/")))))
                '(body "Hey" (& nbsp) (a (@ (href "/"))))))

(define (has-class? name attributes)
  ;; splitting without specifying separator or splitting on #px"\\s+" makes
  ;; string-split use a faster whitespace-specialized implementation.
  (and (member name (string-split (or (get-attribute 'class attributes) "") #px"\\s+")) #t))
(module+ test
  (check-true (has-class? "red" '((class "yellow red blue"))))
  (check-false (has-class? "red" '((class "yellow blue"))))
  (check-false (has-class? "red" '((title "Inside joke.")))))

(define (jp pointer document [else null])
  (with-handlers ([exn:fail:contract? (λ (exn) (cond
                                                 [(null? else) (raise exn)]
                                                 [(procedure? else) (else)]
                                                 [#t else]))])
    (json-pointer-value pointer document)))

(define-syntax-rule (response-handler body ...)
  (with-handlers ([exn:fail? (λ (e)
                               (response/output
                                #:code 500
                                #:mime-type #"text/plain"
                                (λ (out)
                                  (for ([port (list (current-error-port) out)])
                                    (parameterize ([current-error-port port])
                                      (with-handlers ([exn:fail? (λ (e) (void))])
                                        (displayln "Exception raised in Racket code at response generation time:" (current-error-port))
                                        ((error-display-handler) (exn-message e) e)))))))])
    body ...))

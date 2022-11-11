#lang typed/racket/base
(require racket/path
         racket/runtime-path
         racket/string)

(provide
 get-static-url
 link-header)

(define-runtime-path path-static "../static")

(define static-data
  (for/hash : (Immutable-HashTable Path Nonnegative-Integer)([f (directory-list path-static)])
    (define built (simple-form-path (build-path path-static f)))
    (values built (file-or-directory-modify-seconds built))))

(: get-static-url (Path-String -> String))
(define (get-static-url path-or-filename)
  (define the-path (simple-form-path (if (path? path-or-filename)
                                         path-or-filename
                                         (build-path path-static path-or-filename))))
  (format "/static/~a?t=~a" (file-name-from-path the-path) (hash-ref static-data the-path)))

; https://developer.mozilla.org/en-US/docs/Web/HTML/Link_types/preload
(: link-header String)
(define link-header
  (let* ([with-t '(("main.css" "as=style"))]
         [without-t '(("preact.js" "as=script")
                      ("source-sans-pro-v21-vietnamese_latin-ext_latin_greek-ext_greek_cyrillic-ext_cyrillic-regular.woff2" "as=font" "crossorigin" "type=font/woff2"))]
         [with-t-full (map (λ ([path : (Listof String)]) (cons (get-static-url (car path)) (cdr path))) with-t)]
         [without-t-full (map (λ ([path : (Listof String)]) (cons (format "/static/~a" (car path)) (cdr path))) without-t)]
         [all (append with-t-full without-t-full)]
         [header-parts
          (for/list : (Listof String) ([full-path all])
            (define attributes (map (λ ([s : String]) (format "; ~a" s)) (cdr full-path)))
            (format "<~a>; rel=preload~a" (car full-path) (string-join attributes "")))])
    (string-join header-parts ", ")))

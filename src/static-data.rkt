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

(: link-header String)
(define link-header
  (let* ([with-t '("main.css")]
         [with-t-full (map get-static-url with-t)]
         [without-t '("preact.js" "source-sans-pro-v21-vietnamese_latin-ext_latin_greek-ext_greek_cyrillic-ext_cyrillic-regular.woff2")]
         [without-t-full (map (λ (path) (format "/static/~a" path)) without-t)])
    (string-join
     (for/list : (Listof String) ([full-path : String (append with-t-full without-t-full)])
       (format "<~a>; rel=\"preload\"" full-path))
     ", ")))

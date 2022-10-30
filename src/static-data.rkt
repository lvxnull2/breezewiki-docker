#lang typed/racket/base
(require racket/path
         racket/runtime-path)

(provide
 get-static-url)

(define-runtime-path path-static "../static")

(define static-data
  (for/hash ([f (directory-list path-static)]) : (Immutable-HashTable Path Nonnegative-Integer)
    (define built (simple-form-path (build-path path-static f)))
    (values built (file-or-directory-modify-seconds built))))

(: get-static-url ((U String Path) -> String))
(define (get-static-url path-or-filename)
  (define the-path (simple-form-path (if (path? path-or-filename) path-or-filename (build-path path-static path-or-filename))))
  (format "/static/~a?t=~a" (file-name-from-path the-path) (hash-ref static-data the-path)))

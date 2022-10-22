#lang racket/base
(require racket/path
         racket/runtime-path
         net/url
         web-server/http/request-structs
         web-server/servlet-dispatch
         web-server/dispatchers/filesystem-map
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         "config.rkt")

(provide
 static-dispatcher)

(module+ test
  (require rackunit))

(define-runtime-path path-static "../static")

(define hash-ext-mime-type
  (hash #".css" #"text/css"
        #".js" #"text/javascript"
        #".png" #"image/png"
        #".svg" #"image/svg+xml"
        #".txt" #"text/plain"))

(define (ext->mime-type ext)
  (hash-ref hash-ext-mime-type ext))
(module+ test
  (check-equal? (ext->mime-type #".png") #"image/png"))

(define (make-path segments)
  (map (λ (seg) (path/param seg '())) segments))
(module+ test
  (check-equal? (make-path '("static" "main.css"))
                (list (path/param "static" '()) (path/param "main.css" '()))))

(define (path-rewriter p)
  (cond
    ; url is ^/static/... ?
    [(equal? (path/param-path (car p)) "static")
     ; rewrite to ^/... which will be treated as relative to static/ on the filesystem
     (cdr p)]
    ; url is literally ^/robots.txt
    [(equal? p (make-path '("robots.txt")))
     ; rewrite to ^/... -- it already is!
     p]
    ; not going to use the static file dispatcher
    [#t (next-dispatcher)]))
(module+ test
  (check-equal? (path-rewriter (make-path '("static" "main.css")))
                (make-path '("main.css")))
  (check-equal? (path-rewriter (make-path '("static" "robots.txt")))
                (make-path '("robots.txt")))
  (check-equal? (path-rewriter (make-path '("robots.txt")))
                (make-path '("robots.txt"))))

(define (static-dispatcher conn old-req)
  (define old-uri (request-uri old-req))
  (define old-path (url-path old-uri))
  (define new-path (path-rewriter old-path))
  (define new-uri (struct-copy url old-uri [path new-path]))
  (define new-req (struct-copy request old-req [uri new-uri]))
  ((files:make
    #:url->path (lambda (u) ((make-url->path path-static) u))
    #:path->mime-type (lambda (u) (ext->mime-type (path-get-extension u)))
    #:cache-no-cache (config-true? 'debug) #;"browser applies heuristics if unset")
   conn new-req))

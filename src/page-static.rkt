#lang racket/base
(require racket/path
         racket/runtime-path
         net/url
         web-server/http/request-structs
         web-server/servlet-dispatch
         web-server/dispatchers/filesystem-map
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         "../lib/mime-types.rkt"
         "../lib/syntax.rkt"
         "config.rkt")

(provide
 static-dispatcher)

(module+ test
  (require rackunit))

(define-runtime-path path-static "../static")
(define path-archive (anytime-path ".." "storage/archive"))

(define hash-ext-mime-type
  (hash #".css" #"text/css"
        #".js" #"text/javascript"
        #".png" #"image/png"
        #".svg" #"image/svg+xml"
        #".woff2" #"font/woff2"
        #".txt" #"text/plain"))

(define (make-path segments)
  (map (λ (seg) (path/param seg '())) segments))
(module+ test
  (check-equal? (make-path '("static" "main.css"))
                (list (path/param "static" '()) (path/param "main.css" '()))))

;; given a request path, return a rewritten request path and the source directory on the filesystem to serve based on
(define (path-rewriter p)
  (cond
    ; url is ^/static/... ?
    [(equal? (path/param-path (car p)) "static")
     ; rewrite to ^/... which will be treated as relative to static/ on the filesystem
     (values (cdr p) path-static)]
    ; url is ^/archive/... ?
    [(equal? (path/param-path (car p)) "archive")
     ; rewrite req to ^/<wikiname> and dir to /storage/archive
     (values (cdr p) path-archive)]
    ; url is literally ^/robots.txt
    [(equal? p (make-path '("robots.txt")))
     ; rewrite to ^/... -- it already is!
     (values p path-static)]
    ; not going to use the static file dispatcher
    [#t (next-dispatcher)]))
(module+ test
  (check-equal? (call-with-values (λ () (path-rewriter (make-path '("static" "main.css")))) cons)
                (cons (make-path '("main.css")) path-static))
  (check-equal? (call-with-values (λ () (path-rewriter (make-path '("static" "robots.txt")))) cons)
                (cons (make-path '("robots.txt")) path-static))
  (check-equal? (call-with-values (λ () (path-rewriter (make-path '("robots.txt")))) cons)
                (cons (make-path '("robots.txt")) path-static))
  (check-equal? (call-with-values (λ () (path-rewriter (make-path '("archive" "minecraft" "styles" "main.css")))) cons)
                (cons (make-path '("minecraft" "styles" "main.css")) path-archive)))

(define (static-dispatcher conn old-req)
  (define old-uri (request-uri old-req))
  (define old-path (url-path old-uri))
  (define-values (new-path source-dir) (path-rewriter old-path))
  (define new-uri (struct-copy url old-uri [path new-path]))
  (define new-req (struct-copy request old-req [uri new-uri]))
  ((files:make
    #:url->path (lambda (u) ((make-url->path source-dir) u))
    #:path->headers (lambda (p) (list (header #"Access-Control-Allow-Origin" #"*")
                                      (header #"Referrer-Policy" #"same-origin")))
    #:path->mime-type (lambda (u) (ext->mime-type (path-get-extension u)))
    #:cache-no-cache (config-true? 'debug)
    #:cache-immutable (not (config-true? 'debug))
    #:cache-max-age (if (config-true? 'debug) #f 604800))
   conn new-req))

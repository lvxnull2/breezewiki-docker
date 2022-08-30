#lang racket/base
(require racket/path
         racket/runtime-path
         net/url
         web-server/servlet-dispatch
         web-server/dispatchers/filesystem-map
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         "src/config.rkt"
         "src/server-utils.rkt")

(require (only-in "src/page-category.rkt" page-category))
(require (only-in "src/page-home.rkt" page-home))
(require (only-in "src/page-not-found.rkt" page-not-found))
(require (only-in "src/page-proxy.rkt" page-proxy))
(require (only-in "src/page-search.rkt" page-search))
(require (only-in "src/page-wiki.rkt" page-wiki))

(define-runtime-path path-static "static")

(serve/launch/wait
 #:listen-ip (if (config-true? 'debug) "127.0.0.1" #f)
 #:port (string->number (config-get 'port))
 (λ (quit)
   (sequencer:make
    (pathprocedure:make "/" page-home)
    (pathprocedure:make "/proxy" page-proxy)
    (filter:make #rx"^/[a-z-]+/wiki/Category:.+$" (lift:make page-category))
    (filter:make #rx"^/[a-z-]+/wiki/.+$" (lift:make page-wiki))
    (filter:make #rx"^/[a-z-]+/search$" (lift:make page-search))
    (filter:make #rx"^/static/" (files:make
                                 #:url->path
                                 (lambda (u)
                                   ((make-url->path path-static)
                                    (struct-copy url u [path (cdr (url-path u))])))
                                 #:path->mime-type
                                 (lambda (u)
                                   (ext->mime-type (path-get-extension u)))
                                 #:cache-no-cache (config-true? 'debug) #;"browser applies heuristics if unset"))
    (lift:make page-not-found))))

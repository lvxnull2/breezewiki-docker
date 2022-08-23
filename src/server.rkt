#lang racket/base
(require racket/path
         net/url
         web-server/servlet-dispatch
         web-server/dispatchers/filesystem-map
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         (prefix-in files: web-server/dispatchers/dispatch-files)
         "config.rkt"
         "page-category.rkt"
         "page-not-found.rkt"
         "page-proxy.rkt"
         "page-wiki.rkt"
         "page-search.rkt")

(define mime-types
  (hash #".css" #"text/css"
        #".svg" #"image/svg+xml"))

(serve/launch/wait
 #:port (config-get 'port)
 (λ (quit)
   (sequencer:make
    (pathprocedure:make "/proxy" page-proxy)
    (filter:make #rx"^/[a-z-]+/wiki/Category:.+$" (lift:make page-category))
    (filter:make #rx"^/[a-z-]+/wiki/.+$" (lift:make page-wiki))
    (filter:make #rx"^/[a-z-]+/search$" (lift:make page-search))
    (filter:make #rx"^/static/" (files:make
                                 #:url->path
                                 (lambda (u)
                                   ((make-url->path "../static")
                                    (struct-copy url u [path (cdr (url-path u))])))
                                 #:path->mime-type
                                 (lambda (u)
                                   (hash-ref mime-types (path-get-extension u)))
                                 #:cache-no-cache (config-get 'debug) #;"browser applies heuristics if unset"))
    (lift:make page-not-found))))

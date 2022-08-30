#lang racket/base
(require web-server/servlet-dispatch
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         "src/config.rkt"
         "src/reloadable.rkt")

(define-syntax-rule (require-reloadable filename varname)
  (define varname
    (reloadable-entry-point->procedure
     (make-reloadable-entry-point (quote varname) filename))))

(require-reloadable "src/page-category.rkt" page-category)
(require-reloadable "src/page-home.rkt" page-home)
(require-reloadable "src/page-not-found.rkt" page-not-found)
(require-reloadable "src/page-proxy.rkt" page-proxy)
(require-reloadable "src/page-search.rkt" page-search)
(require-reloadable "src/page-static.rkt" static-dispatcher)
(require-reloadable "src/page-wiki.rkt" page-wiki)

(when (not (config-true? 'debug))
  (set-reload-poll-interval! #f))
(reload!)

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
    static-dispatcher
    (lift:make page-not-found))))

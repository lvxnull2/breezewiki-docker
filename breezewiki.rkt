#lang racket/base
(require web-server/servlet-dispatch
         "src/config.rkt"
         "src/dispatcher-tree.rkt"
         "src/reloadable.rkt")

(define-syntax-rule (require-reloadable filename varname)
  (define varname
    (reloadable-entry-point->procedure
     (make-reloadable-entry-point (quote varname) filename))))

(require-reloadable "src/page-category.rkt" page-category)
(require-reloadable "src/page-global-search.rkt" page-global-search)
(require-reloadable "src/page-home.rkt" page-home)
(require-reloadable "src/page-it-works.rkt" page-it-works)
(require-reloadable "src/page-not-found.rkt" page-not-found)
(require-reloadable "src/page-proxy.rkt" page-proxy)
(require-reloadable "src/page-redirect-wiki-home.rkt" redirect-wiki-home)
(require-reloadable "src/page-search.rkt" page-search)
(require-reloadable "src/page-set-user-settings.rkt" page-set-user-settings)
(require-reloadable "src/page-static.rkt" static-dispatcher)
(require-reloadable "src/page-static-archive.rkt" page-static-archive)
(require-reloadable "src/page-subdomain.rkt" subdomain-dispatcher)
(require-reloadable "src/page-wiki.rkt" page-wiki)
(require-reloadable "src/page-wiki-offline.rkt" page-wiki-offline)
(require-reloadable "src/page-file.rkt" page-file)

(reload!)

(define ch (make-channel))
(define (start)
  (serve/launch/wait
   #:listen-ip (if (equal? (config-get 'bind_host) "auto")
                 (if (config-true? 'debug) "127.0.0.1" #f)
                 (config-get 'bind_host))
   #:port (string->number (config-get 'port))
   (λ (quit)
     (channel-put ch (lambda () (semaphore-post quit)))
     (dispatcher-tree
      ; order of these does not matter
      page-category
      page-global-search
      page-home
      page-it-works
      page-not-found
      page-proxy
      page-search
      page-set-user-settings
      page-static-archive
      page-wiki
      page-wiki-offline
      page-file
      redirect-wiki-home
      static-dispatcher
      subdomain-dispatcher))))
(define server-t (thread start))
(define quit (channel-get ch))

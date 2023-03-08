#lang racket/base
(require web-server/servlet-dispatch
         "src/config.rkt"
         "src/dispatcher-tree.rkt")

(require (only-in "src/page-category.rkt" page-category))
(require (only-in "src/page-global-search.rkt" page-global-search))
(require (only-in "src/page-home.rkt" page-home))
(require (only-in "src/page-it-works.rkt" page-it-works))
(require (only-in "src/page-not-found.rkt" page-not-found))
(require (only-in "src/page-proxy.rkt" page-proxy))
(require (only-in "src/page-redirect-wiki-home.rkt" redirect-wiki-home))
(require (only-in "src/page-search.rkt" page-search))
(require (only-in "src/page-set-user-settings.rkt" page-set-user-settings))
(require (only-in "src/page-static.rkt" static-dispatcher))
(require (only-in "src/page-static-archive.rkt" page-static-archive))
(require (only-in "src/page-subdomain.rkt" subdomain-dispatcher))
(require (only-in "src/page-wiki.rkt" page-wiki))
(require (only-in "src/page-wiki-offline.rkt" page-wiki-offline))
(require (only-in "src/page-file.rkt" page-file))

(serve/launch/wait
 #:listen-ip (if (equal? (config-get 'bind_host) "auto")
                 (if (config-true? 'debug) "127.0.0.1" #f)
                 (config-get 'bind_host))
 #:port (string->number (config-get 'port))
 (λ (quit)
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
    subdomain-dispatcher)))

#lang racket/base
(require (for-syntax racket/base)
         racket/string
         net/url
         (prefix-in host: web-server/dispatchers/dispatch-host)
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter)
         "config.rkt")

(provide
 ; syntax to make the hashmap from names
 dispatcher-tree
 ; procedure to make the tree from the hashmap
 make-dispatcher-tree)

(define-syntax (if/out stx)
  (define tree (cdr (syntax->datum stx))) ; condition true false
  (define else (cddr tree)) ; the else branch cons cell
  (define result
   (let walk ([node tree])
     (cond
       ; normally, node should be a full cons cell (a pair) but it might be something else.
       ; situation: reached the end of a list, empty cons cell
       [(null? node) node]
       ; situation: reached the end of a list, cons cdr was non-list
       [(symbol? node) node]
       ; normal situation, full cons cell
       ; -- don't go replacing through nested if/out
       [(and (pair? node) (eq? 'if/out (car node))) node]
       ; -- replace if/in
       [(and (pair? node) (eq? 'if/in (car node)))
        (append '(if) (cdr node) else)]
       ; recurse down pair head and tail
       [(pair? node) (cons (walk (car node)) (walk (cdr node)))]
       ; something else that can't be recursed into, so pass it through
       [#t node])))
  (datum->syntax stx (cons 'if result)))

; make a hashmap out of the provided names and call make-dispatcher-tree with it
(define-syntax (dispatcher-tree stx)
  ; the arguments, which are names of dispatcher variables
  (define names (cdr (syntax->list stx)))
  ; map each name to syntax of a '(name . ,name)
  (define alist (map (λ (xe) ; xe is the syntax of a name
                       ; return instead syntax of a cons cell
                       (datum->syntax stx `(cons ',xe ,xe)))
                     names))
  ; make syntax to make the hash
  (define ds (datum->syntax stx `(make-hasheq (list ,@alist))))
  ; don't forget that I'm returning *code* - return a call to the function
  (datum->syntax stx `(make-dispatcher-tree ,ds)))

(define (make-dispatcher-tree ds)
  (host:make
   (λ (host-sym)
     (if/out (config-true? 'canonical_origin)
             (let* ([host-header (symbol->string host-sym)]
                    [splitter (string-append "." (url-host (string->url (config-get 'canonical_origin))))]
                    [s (string-split host-header splitter #:trim? #f)])
               (if/in (and (eq? 2 (length s)) (equal? "" (cadr s)))
                      ((hash-ref ds 'subdomain-dispatcher) (car s))))
             (sequencer:make
              (pathprocedure:make "/" (hash-ref ds 'page-home))
              (pathprocedure:make "/proxy" (hash-ref ds 'page-proxy))
              (filter:make #rx"^/[a-z-]+/wiki/Category:.+$" (lift:make (hash-ref ds 'page-category)))
              (filter:make #rx"^/[a-z-]+/wiki/.+$" (lift:make (hash-ref ds 'page-wiki)))
              (filter:make #rx"^/[a-z-]+/search$" (lift:make (hash-ref ds 'page-search)))
              (hash-ref ds 'static-dispatcher)
              (lift:make (hash-ref ds 'page-not-found)))))))

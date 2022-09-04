#lang racket/base
(require (for-syntax racket/base)
         (prefix-in pathprocedure: web-server/dispatchers/dispatch-pathprocedure)
         (prefix-in sequencer: web-server/dispatchers/dispatch-sequencer)
         (prefix-in lift: web-server/dispatchers/dispatch-lift)
         (prefix-in filter: web-server/dispatchers/dispatch-filter))

(provide
 ; syntax to make the hashmap from names
 dispatcher-tree
 ; procedure to make the tree from the hashmap
 make-dispatcher-tree)

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
  (sequencer:make
   (pathprocedure:make "/" (hash-ref ds 'page-home))
   (pathprocedure:make "/proxy" (hash-ref ds 'page-proxy))
   (filter:make #rx"^/[a-z-]+/wiki/Category:.+$" (lift:make (hash-ref ds 'page-category)))
   (filter:make #rx"^/[a-z-]+/wiki/.+$" (lift:make (hash-ref ds 'page-wiki)))
   (filter:make #rx"^/[a-z-]+/search$" (lift:make (hash-ref ds 'page-search)))
   (hash-ref ds 'static-dispatcher)
   (lift:make (hash-ref ds 'page-not-found))))

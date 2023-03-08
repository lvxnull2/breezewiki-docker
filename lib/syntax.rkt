#lang racket/base
(require (for-syntax racket/base syntax/location))

(provide
 ; help make a nested if. if/in will gain the same false form of its containing if/out.
 if/out
 ; cond, but values can be defined between conditions
 cond/var
 ; wrap sql statements into lambdas so they can be executed during migration
 wrap-sql
 ; get the name of the file that contains the currently evaluating form
 this-directory
 this-file
 ; replacement for define-runtime-path
 anytime-path)

(module+ test
  (require rackunit)
  (define (check-syntax-equal? s1 s2)
    (check-equal? (syntax->datum s1)
                  (syntax->datum s2))))

;; actual transforming goes on in here.
;; it's in a submodule so that it can be required in both levels, for testing

(module transform racket/base
  (require racket/list)

  (provide
   transform-if/out
   transform/out-cond/var)

  (define (transform-if/out stx)
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
           (append '(if) (walk (cdr node)) else)]
          ; recurse down pair head and tail
          [(pair? node) (cons (walk (car node)) (walk (cdr node)))]
          ; something else that can't be recursed into, so pass it through
          [#t node])))
    (datum->syntax stx (cons 'if result)))

  (define (transform/out-cond/var stx)
    (define tree (transform-cond/var (cdr (syntax->datum stx))))
    (datum->syntax
     stx
     tree))

  (define (transform-cond/var tree)
    (define-values (els temp) (splitf-at tree (λ (el) (and (pair? el) (not (eq? (car el) 'var))))))
    (define-values (vars rest) (splitf-at temp (λ (el) (and (pair? el) (eq? (car el) 'var)))))
    (if (null? rest)
        `(cond ,@els)
        `(cond
           ,@els
           [#t
            (let* ,(for/list ([var vars])
                     (cdr var))
              ,(transform-cond/var rest))]))))

;; the syntax definitions and their tests go below here

(require 'transform (for-syntax 'transform))

(define-syntax (wrap-sql stx)
  ; the arguments
  (define xs (cdr (syntax->list stx)))
  ; wrap each argument
  (define wrapped (map (λ (xe) ; xe is the syntax of an argument
                         (if (list? (car (syntax->datum xe)))
                             ; it's a list of lists (a list of sql migration steps)
                             ; return instead syntax of a lambda that will call everything in xe
                             (datum->syntax stx `(λ () ,@xe))
                             ; it's just a single sql migration step
                             ; return instead syntax of a lambda that will call xe
                             (datum->syntax stx `(λ () ,xe))))
                       xs))
  ; since I'm returning *code*, I need to return the form (list ...) so that runtime makes a list
  (datum->syntax stx `(list ,@wrapped)))

(define-syntax (if/out stx)
  (transform-if/out stx))
(module+ test
  (check-syntax-equal? (transform-if/out #'(if/out (condition 1) (if/in (condition 2) (do-yes)) (do-no)))
                       #'(if (condition 1) (if (condition 2) (do-yes) (do-no)) (do-no)))
  (check-equal? (if/out #t (if/in #t 'yes) 'no) 'yes)
  (check-equal? (if/out #f (if/in #t 'yes) 'no) 'no)
  (check-equal? (if/out #t (if/in #f 'yes) 'no) 'no)
  (check-equal? (if/out #f (if/in #f 'yes) 'no) 'no))

(define-syntax (this-directory stx)
  (datum->syntax stx (syntax-source-directory stx)))

(define-syntax (this-file stx)
  (datum->syntax stx (build-path (or (syntax-source-directory stx) 'same) (syntax-source-file-name stx))))

(module+ test
  (require racket/path)
  (check-equal? (file-name-from-path (this-file)) (build-path "syntax.rkt")))

(define-syntax (cond/var stx)
  (transform/out-cond/var stx))
(module+ test
  (check-syntax-equal? (transform/out-cond/var #'(cond/def [#f 0] (var d (* a 2)) [(eq? d 8) d] [#t "not 4"]))
                       #'(cond
                           [#f 0]
                           [#t
                            (let* ([d (* a 2)])
                              (cond
                                [(eq? d 8) d]
                                [#t "not 4"]))])))

;;; Replacement for define-runtime-path that usually works well and doesn't include the files/folder contents into the distribution.
;;; When running from source, should always work appropriately.
;;; When running from a distribution, (current-directory) is treated as the root.
;;; Usage:
;;;   * to-root : Path-String * relative path from the source file to the project root
;;;   * to-dest : Path-String * relative path from the root to the desired file/folder
(define-syntax (anytime-path stx)
  (define-values (_ to-root to-dest) (apply values (syntax->list stx)))
  (define source (syntax-source stx))
  (unless (complete-path? source)
    (error 'anytime-path "syntax source has no directory: ~v" stx))
  (datum->syntax
   stx
   `(let* ([source ,source]
           [dir-of-source (path-only source)]
           [_ (unless (path? dir-of-source) (error 'anytime-path "syntax source has no directory: ~v" ,source))]
           [syntax-to-root (build-path dir-of-source ,to-root)]
           [root (if (directory-exists? syntax-to-root)
                     ;; running on the same filesystem it was compiled on, i.e. it's running the source code out of a directory, and the complication is the intermediate compilation
                     syntax-to-root
                     ;; not running on the same filesystem, i.e. it's a distribution. we assume that the current working directory is where the executable is, and treat this as the root.
                     (current-directory))])
      (simple-form-path (build-path root ,to-dest)))))

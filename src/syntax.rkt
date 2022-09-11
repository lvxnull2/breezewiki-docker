#lang racket/base
(require (for-syntax racket/base))

(provide
 ; help make a nested if where the false results are the same
 if/out)

(module+ test
  (require rackunit)
  (define (check-syntax-equal? s1 s2)
    (check-equal? (syntax->datum s1)
                  (syntax->datum s2))))

;; actual transforming goes on in here.
;; it's in a submodule so that it can be required in both levels, for testing

(module transform racket/base
  (provide transform-if/out)
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
           (append '(if) (cdr node) else)]
          ; recurse down pair head and tail
          [(pair? node) (cons (walk (car node)) (walk (cdr node)))]
          ; something else that can't be recursed into, so pass it through
          [#t node])))
    (datum->syntax stx (cons 'if result))))

;; the syntax definitions and their tests go below here

(require 'transform (for-syntax 'transform))

(define-syntax (if/out stx)
  (transform-if/out stx))
(module+ test
  (check-syntax-equal? (transform-if/out #'(if/out (condition 1) (if/in (condition 2) (do-yes)) (do-no)))
                       #'(if (condition 1) (if (condition 2) (do-yes) (do-no)) (do-no)))
  (check-equal? (if/out #t (if/in #t 'yes) 'no) 'yes)
  (check-equal? (if/out #f (if/in #t 'yes) 'no) 'no)
  (check-equal? (if/out #t (if/in #f 'yes) 'no) 'no)
  (check-equal? (if/out #f (if/in #f 'yes) 'no) 'no))

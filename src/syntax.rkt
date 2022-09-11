#lang racket/base
(require (for-syntax racket/base))

(provide
 ; help make a nested if. if/in will gain the same false form of its containing if/out.
 if/out
 ; let, but the value for each variable is evaluated within a thread
 thread-let)

(module+ test
  (require rackunit)
  (define (check-syntax-equal? s1 s2)
    (check-equal? (syntax->datum s1)
                  (syntax->datum s2))))

;; actual transforming goes on in here.
;; it's in a submodule so that it can be required in both levels, for testing

(module transform racket/base
  (provide
   transform-if/out
   transform-thread-let)

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
    (datum->syntax stx (cons 'if result)))

  (define (transform-thread-let stx)
    (define tree (cdr (syntax->datum stx)))
    (define defs (car tree))
    (define forms (cdr tree))
    (when (eq? (length forms) 0)
      (error (format "thread-let: bad syntax (need some forms to execute after the threads)~n  forms: ~a" forms)))
    (define counter (build-list (length defs) values))
    (datum->syntax
     stx
     `(let ([chv (build-vector ,(length defs) (λ (_) (make-channel)))])
        ,@(map (λ (n)
                 (define def (list-ref defs n))
                 `(thread (λ () (channel-put (vector-ref chv ,n) (let _ () ,@(cdr def))))))
               counter)
        (let ,(map (λ (n)
                     (define def (list-ref defs n))
                     `(,(car def) (channel-get (vector-ref chv ,n))))
                   counter)
            ,@forms)))))

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

(define-syntax (thread-let stx)
  (transform-thread-let stx))
(module+ test
  ; check that it is transformed as expected
  (check-syntax-equal?
   (transform-thread-let
    #'(thread-let ([a (hey "this is a")]
                   [b (hey "this is b")])
                  (list a b)))
   #'(let ([chv (build-vector 2 (λ (_) (make-channel)))])
       (thread (λ () (channel-put (vector-ref chv 0) (let _ () (hey "this is a")))))
       (thread (λ () (channel-put (vector-ref chv 1) (let _ () (hey "this is b")))))
       (let ([a (channel-get (vector-ref chv 0))]
             [b (channel-get (vector-ref chv 1))])
         (list a b))))
  ; check that they actually execute concurrently
  (define ch (make-channel))
  (check-equal? (thread-let ([a (begin
                                  (channel-put ch 'a)
                                  (channel-get ch))]
                             [b (begin0
                                    (channel-get ch)
                                  (channel-put ch 'b))])
                            (list a b))
                '(b a))
  ; check that it assigns the correct value to the correct variable
  (check-equal? (thread-let ([a (sleep 0) 'a] [b 'b]) (list a b))
                '(a b)))

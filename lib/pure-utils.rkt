#lang typed/racket/base

(provide
 ; call the updater on the dictionary key only if it has that key
 alist-maybe-update
 ; update a value only if a condition succeeds on it
 u
 ; like string-join, but for lists
 list-join
 u-counter)

(module+ test
  (require "typed-rackunit.rkt"))

(define u-counter (box 0))

(: alist-maybe-update (∀ (A B) ((Listof (Pairof A B)) A (B -> B) -> (Listof (Pairof A B)))))
(define (alist-maybe-update alist key updater)
  (set-box! u-counter (add1 (unbox u-counter)))
  (map (λ ([p : (Pairof A B)])
         (if (eq? (car p) key)
             (cons (car p) (updater (cdr p)))
             p))
       alist))
(module+ test
  (check-equal? (alist-maybe-update '((a . 5) (b . 6)) 'a (λ ([x : Number]) (+ x 10)))
                '((a . 15) (b . 6)))
  (check-equal? (alist-maybe-update '((b . 6)) 'a (λ ([x : Number]) (+ x 10)))
                '((b . 6))))

(: u (∀ (A) ((A -> Any) (A -> A) A -> A)))
(define (u condition updater value)
  (set-box! u-counter (add1 (unbox u-counter)))
  (if (condition value) (updater value) value))
(module+ test
  (check-equal? (u (λ ([x : Integer]) (< x 5)) (λ ([x : Integer]) (* x -1)) 4) -4)
  (check-equal? (u (λ ([x : Integer]) (< x 5)) (λ ([x : Integer]) (* x -1)) 8) 8))

(: list-join (∀ (A B) (A (Listof B) -> (Listof (U A B)))))
(define (list-join element ls)
  (if (pair? (cdr ls))
      (list* (car ls) element (list-join element (cdr ls)))
      (list (car ls))))
(module+ test
  (check-equal? (list-join "h" '(2 3 4 5)) '(2 "h" 3 "h" 4 "h" 5)))

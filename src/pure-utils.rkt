#lang typed/racket/base

(provide
 ; call the updater on the dictionary key only if it has that key
 alist-maybe-update
 ; update a value only if a condition succeeds on it
 u)

(module+ test
  (require "typed-rackunit.rkt"))

(: alist-maybe-update (∀ (A B) ((Listof (Pairof A B)) A (B -> B) -> (Listof (Pairof A B)))))
(define (alist-maybe-update alist key updater)
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
  (if (condition value) (updater value) value))
(module+ test
  (check-equal? (u (λ ([x : Integer]) (< x 5)) (λ ([x : Integer]) (* x -1)) 4) -4)
  (check-equal? (u (λ ([x : Integer]) (< x 5)) (λ ([x : Integer]) (* x -1)) 8) 8))

#lang typed/racket/base

(provide
 check-equal?
 check-true
 check-false)

(require/typed rackunit
  [check-equal? (Any Any -> Void)]
  [check-true (Any -> Void)]
  [check-false (Any -> Void)])

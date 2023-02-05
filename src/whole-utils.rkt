#lang typed/racket/base
(require "config.rkt")

(provide
 ; prints "out: <url>"
 log-outgoing)

(: log-outgoing (String -> Void))
(define (log-outgoing url-string)
  (when (config-true? 'log_outgoing)
    (printf "out: ~a~n" url-string)))

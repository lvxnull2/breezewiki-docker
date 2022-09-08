#lang racket/base
(require racket/pretty
         racket/runtime-path
         ini)

(provide
 config-parameter
 config-true?
 config-get)

(define-runtime-path path-config "../config.ini")

(define (config-parameter key)
  (hash-ref config key))

(define (config-true? key)
  (not (member ((config-parameter key)) '("" "false"))))

(define (config-get key)
  ((config-parameter key)))

(define default-config
  '((application_name . "BreezeWiki")
    (canonical_origin . "")
    (debug . "false")
    (instance_is_official . "false") ; please don't turn this on, or you will make me very upset
    (port . "10416")
    (strict_proxy . "true")))

(define loaded-alist
  (with-handlers
    ([exn:fail:filesystem:errno?
      (λ (exn)
        (begin0
            '()
          (displayln "note: config file not detected, using defaults")))]
     [exn:fail:contract?
      (λ (exn)
        (begin0
            '()
          (displayln "note: config file empty or missing [] section, using defaults")))])
    (define l
      (hash->list
       (hash-ref
        (ini->hash
         (call-with-input-file path-config
           (λ (in)
             (read-ini in))))
        '||)))
    (begin0
        l
      (printf "note: ~a items loaded from config file~n" (length l)))))

(define combined-alist (append default-config loaded-alist))

(define config
  (make-hasheq
   (map (λ (pair)
          (cons (car pair) (make-parameter (cdr pair))))
        combined-alist)))

(when (config-true? 'debug)
  ; all values here are optimised for maximum prettiness
  (parameterize ([pretty-print-columns 80])
    (display "config: ")
    (pretty-write (sort
                   (hash->list (make-hasheq combined-alist))
                   symbol<?
                   #:key car))))

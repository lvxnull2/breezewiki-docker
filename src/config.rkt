#lang racket/base
(require racket/function
         racket/pretty
         racket/runtime-path
         racket/string
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

(define env-alist
  (let ([e-names (environment-variables-names (current-environment-variables))]
        [e-ref (λ (name) (bytes->string/latin-1 (environment-variables-ref (current-environment-variables) name)))])
    (map (λ (name) (cons (string->symbol (string-downcase (substring (bytes->string/latin-1 name) 3)))
                         (e-ref name)))
         (filter (λ (name) (string-prefix? (string-downcase (bytes->string/latin-1 name)) "bw_")) e-names))))
(when (> (length env-alist) 0)
  (printf "note: ~a items loaded from environment variables~n" (length env-alist)))

(define combined-alist (append default-config loaded-alist env-alist))

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

(when (not (config-true? 'debug))
  (when (not (config-true? 'canonical_origin))
    (displayln
     (string-append "warning: configuring canonical_origin is highly recommended for production!\n"
                    "         see https://docs.breezewiki.com/Configuration.html"))))

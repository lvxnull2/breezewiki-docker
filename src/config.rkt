#lang typed/racket/base
(require racket/function
         racket/pretty
         racket/runtime-path
         racket/string)
(require/typed ini
  [#:opaque Ini ini?]
  [read-ini (Input-Port -> Ini)]
  [ini->hash (Ini -> (Immutable-HashTable Symbol (Immutable-HashTable Symbol String)))])

(provide
 config-parameter
 config-true?
 config-get)

(module+ test
  (require "typed-rackunit.rkt"))

(define-runtime-path path-config "../config.ini")

(: config-parameter (Symbol -> (Parameterof String)))
(define (config-parameter key)
  (hash-ref config key))

(: config-true? (Symbol -> Boolean))
(define (config-true? key)
  (not (member ((config-parameter key)) '("" "false"))))

(: config-get (Symbol -> String))
(define (config-get key)
  ((config-parameter key)))

(define default-config
  '((application_name . "BreezeWiki")
    (canonical_origin . "")
    (debug . "false")
    (instance_is_official . "false") ; please don't turn this on, or you will make me very upset
    (log_outgoing . "true")
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
        [e-ref (λ ([name : Bytes])
                 (bytes->string/latin-1
                  (cast (environment-variables-ref (current-environment-variables) name)
                        Bytes)))])
    (map (λ ([name : Bytes])
           (cons (string->symbol (string-downcase (substring (bytes->string/latin-1 name) 3)))
                 (e-ref name)))
         (filter (λ ([name : Bytes]) (string-prefix? (string-downcase (bytes->string/latin-1 name))
                                                     "bw_"))
                 e-names))))
(when (> (length env-alist) 0)
  (printf "note: ~a items loaded from environment variables~n" (length env-alist)))

(define combined-alist (append default-config loaded-alist env-alist))

(define config
  (make-immutable-hasheq
   (map (λ ([pair : (Pairof Symbol String)])
          (cons (car pair) (make-parameter (cdr pair))))
        combined-alist)))

(when (config-true? 'debug)
  ; all values here are optimised for maximum prettiness
  (parameterize ([pretty-print-columns 80])
    (display "config: ")
    (pretty-write ((inst sort (Pairof Symbol String))
                   (hash->list (make-immutable-hasheq combined-alist))
                   symbol<?
                   #:key car))))

(when (not (config-true? 'debug))
  (when (not (config-true? 'canonical_origin))
    (displayln
     (string-append "warning: configuring canonical_origin is highly recommended for production!\n"
                    "         see https://docs.breezewiki.com/Configuration.html"))))

(module+ test
  ; this is just a sanity check
  (parameterize ([(config-parameter 'application_name) "JeffWiki"]
                 [(config-parameter 'strict_proxy) ""])
    (check-equal? (config-get 'application_name) "JeffWiki")
    (check-false (config-true? 'strict_proxy))))

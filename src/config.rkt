#lang typed/racket/base
(require racket/function
         racket/pretty
         racket/runtime-path
         racket/string
         typed/ini)

(provide
 config-parameter
 config-true?
 config-member?
 config-get)

(module+ test
  (require "../lib/typed-rackunit.rkt"))

(define-runtime-path path-config "../config.ini")

(: config-parameter (Symbol -> (Parameterof String)))
(define (config-parameter key)
  (hash-ref config key))

(: config-true? (Symbol -> Boolean))
(define (config-true? key)
  (not (member ((config-parameter key)) '("" "false"))))

(: config-member? (Symbol String [#:sep String] -> Boolean))
(define (config-member? key item #:sep [sep #px"\\s+"])
  (and (config-true? key)
       (not (not (member item (string-split (config-get key) sep))))))

(: config-get (Symbol -> String))
(define (config-get key)
  ((config-parameter key)))

(define default-config
  '((application_name . "BreezeWiki")
    (bind_host . "auto")
    (port . "10416")
    (canonical_origin . "")
    (debug . "false")
    (feature_search_suggestions . "true")
    (instance_is_official . "false") ; please don't turn this on, or you will make me very upset
    (log_outgoing . "true")
    (strict_proxy . "false")

    (feature_offline::enabled . "false")
    (feature_offline::format . "json.gz")
    (feature_offline::only . "false")

    (access_log::enabled . "false")

    (promotions::indie_wiki_buddy . "banner home")))

(define loaded-alist
  (with-handlers
    ([exn:fail:filesystem:errno?
      (λ (exn)
        (displayln "note: config file not detected, using defaults")
        '())]
     [exn:fail:contract?
      (λ (exn)
        (displayln "note: config file empty or missing [] section, using defaults")
        '())])
    (define h (in-hash
               (ini->hash
                (call-with-input-file path-config
                  (λ (in)
                    (read-ini in))))))
    (define l
      (for*/list : (Listof (Pairof Symbol String))
                 ([(section-key section) h]
                  [(key value) (in-hash section)])
        (if (eq? section-key '||)
            (cons key value)
            (cons (string->symbol (string-append (symbol->string section-key)
                                                 "::"
                                                 (symbol->string key)))
                  value))))
    (printf "note: ~a items loaded from config file~n" (length l))
    l))

(define env-alist
  (for/list : (Listof (Pairof Symbol String))
            ([name (environment-variables-names (current-environment-variables))]
             #:when (string-prefix? (string-downcase (bytes->string/latin-1 name)) "bw_"))
    (cons
     ;; key: convert to string, remove bw_ prefix, convert to symbol
     (string->symbol (string-downcase (substring (bytes->string/latin-1 name) 3)))
     ;; value: convert to string
     (bytes->string/latin-1
      (cast (environment-variables-ref (current-environment-variables) name) Bytes)))))
(when (> (length env-alist) 0)
  (printf "note: ~a items loaded from environment variables~n" (length env-alist)))

(define combined-alist (append default-config loaded-alist env-alist))

(define config
  (for/hasheq ([pair combined-alist]) : (Immutable-HashTable Symbol (Parameter String))
    (values (car pair) (make-parameter (cdr pair)))))

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
                 [(config-parameter 'strict_proxy) ""]
                 [(config-parameter 'promotions::indie_wiki_buddy) "a b c"])
    (check-equal? (config-get 'application_name) "JeffWiki")
    (check-false (config-true? 'strict_proxy))
    (check-equal? (string? (config-get 'feature_offline::format)) #t)
    (check-true (config-member? 'promotions::indie_wiki_buddy "b"))))


#lang racket/base
(require racket/pretty
         racket/runtime-path
         ini)

(provide
 config-true?
 config-get)

(define-runtime-path path-config "../config.ini")

(define (config-true? key)
  (not (member (hash-ref config key) '("" "false"))))

(define (config-get key)
  (hash-ref config key))

(define default-config
  '((port . "10416")
    (debug . "false")
    (instance-is-official . "false") ; please don't turn this on, or you will make me very upset
    (application-name . "BreezeWiki")))

(define config
  (make-hasheq
   (append
    default-config
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
        (printf "note: ~a items loaded from config file~n" (length l)))))))

(when (config-true? 'debug)
  ; all values here are optimised for maximum prettiness
  (parameterize ([pretty-print-columns 80])
    (display "config: ")
    (pretty-write (hash->list config))))

#lang racket/base
(require racket/contract
         racket/match
         racket/path
         racket/runtime-path
         racket/string)

(provide
 (contract-out
  [ext->mime-type (-> bytes? bytes?)]
  [mime-type->ext (-> bytes? bytes?)]))

(define-runtime-path mime.types-path "mime.types")

(define ls
  (call-with-input-file mime.types-path
    (λ (in) (for/list ([line (in-lines in)]
                       #:when (not (regexp-match? #rx"^ *($|#)" line)))
              (match line
                [(regexp #rx"^([^ ]+) +(.+)$" (list _ mime ext))
                 (cons (string->bytes/utf-8 ext) (string->bytes/utf-8 mime))]
                [(regexp #rx"^ *#") (void)]
                [_ (log-warning "mime-types: failed to parse line ~s" line)])))))

(define forward-hash (make-immutable-hash ls))
(define reverse-hash (make-immutable-hash (map (λ (x) (cons (cdr x) (car x))) ls)))

(define (ext->mime-type ext-in)
  (define ext (regexp-replace #rx"^\\." ext-in #""))
  (hash-ref forward-hash ext))

(define (mime-type->ext m-in)
  (define m (regexp-replace #rx";.*" m-in #""))
  (hash-ref reverse-hash m))

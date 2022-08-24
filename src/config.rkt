#lang racket/base
(require racket/runtime-path)

(provide
 config-get)

(define-runtime-path path-config "../config.txt")

(define (config-get key)
  (hash-ref config key))

(define default-config
  '((port . 10416)
    (debug . #f)
    (instance-is-official . #f) ; please don't turn this on, or you will make me very upset
    (application-name . "BreezeWiki")))

(define config
  (make-hasheq
   (append
    default-config
    (with-handlers ([exn:fail:filesystem:errno? (λ (exn)
                                                  '())])
      (call-with-input-file path-config
        (λ (in)
          (let loop ([alist '()])
            (let ([key (read in)]
                  [value (read in)])
              (if (eq? value eof)
                  alist
                  (loop (cons (cons key
                                    (cond
                                      [(eq? value 'true) #t]
                                      [(eq? value 'false) #f]
                                      [#t value]))
                              alist)))))))))))

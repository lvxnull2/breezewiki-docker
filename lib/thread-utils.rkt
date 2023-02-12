#lang racket/base
(require (prefix-in easy: net/http-easy)
         "../src/data.rkt"
         "xexpr-utils.rkt")

(provide
 thread-values)

(module+ test
  (require rackunit))

(define (thread-values . thunks)
  (parameterize-break #t
    (define the-exn (box #f))
    (define original-thread (current-thread))
    (define (break e)
      (when (box-cas! the-exn #f e)
        (break-thread original-thread))
      (sleep 0))
    (define-values (threads channels)
      (for/fold ([ts null]
                 [chs null]
                 #:result (values (reverse ts) (reverse chs)))
                ([th thunks])
        (define ch (make-channel))
        (define t
          (thread (λ ()
                    (with-handlers ([exn? break])
                      (channel-put ch (th))))))
        (values (cons t ts) (cons ch chs))))
    (apply
     values
     (with-handlers ([exn:break? (λ (_)
                                   (for ([t threads]) (kill-thread t))
                                   (if (unbox the-exn)
                                       (raise (unbox the-exn))
                                       (error 'thread-values "a thread broke, but without reporting its exception")))])
       (for/list ([ch channels])
         (channel-get ch))))))

(module+ test
  ; check that they actually execute concurrently
  (define ch (make-channel))
  (check-equal? (let-values ([(a b)
                              (thread-values
                               (λ ()
                                 (begin
                                   (channel-put ch 'a)
                                   (channel-get ch)))
                               (λ ()
                                 (begin0
                                     (channel-get ch)
                                   (channel-put ch 'b))))])
                  (list a b))
                '(b a))
  ; check that it assigns the correct value to the correct variable
  (check-equal? (let-values ([(a b)
                              (thread-values
                               (λ () (sleep 0) 'a)
                               (λ () 'b))])
                  (list a b))
                '(a b))
  ; check that exceptions are passed to the original thread, and other threads are killed
  ;; TODO: if the other thread was making an HTTP request, could it be left stuck open by the kill?
  (check-equal? (let* ([x "!"]
                       [res
                        (with-handlers ([exn:fail:user? (λ (e) (exn-message e))])
                          (thread-values
                           (λ () (sleep 0) (set! x "?") (println "this side effect should not happen"))
                           (λ () (raise-user-error "catch me"))))])
                  (string-append res x))
                "catch me!"))

#lang typed/racket/base
(require racket/file
         racket/path
         racket/port
         racket/string
         typed/srfi/19
         "config.rkt")

(provide
 log-page-request
 log-styles-request
 log-set-settings-request)

(define last-flush 0)
(define flush-every-millis 60000)

;; anytime-path macro expansion only works in an untyped submodule for reasons I cannot comprehend
(module define-log-dir racket/base
  (require racket/path
           "../lib/syntax.rkt")
  (provide log-dir)
  (define log-dir (anytime-path ".." "storage/logs")))
(require/typed (submod "." define-log-dir)
               [log-dir Path])

(define log-file (build-path log-dir "access-0.log"))
(define log-port
  (if (config-true? 'access_log::enabled)
      (begin
        (make-directory* log-dir)
        (open-output-file log-file #:exists 'append))
      (open-output-nowhere)))

(: get-date-iso8601 (-> String))
(define (get-date-iso8601)
  (date->string (current-date 0) "~5"))

(: offline-string (Boolean -> String))
(define (offline-string offline?)
  (if offline? "---" "ooo"))

(: log (String * -> Void))
(define (log . entry)
  ;; create log entry string
  (define full-entry (cons (get-date-iso8601) entry))
  ;; write to output port
  (displayln (string-join full-entry ";") log-port)
  ;; flush output port to file (don't do this too frequently)
  (when ((- (current-milliseconds) last-flush) . >= . flush-every-millis)
    (flush-output log-port)
    (set! last-flush (current-milliseconds))))

(: log-page-request (Boolean String String (U 'light 'dark 'default) -> Void))
(define (log-page-request offline? wikiname title theme)
  (log "page" (offline-string offline?) wikiname title (symbol->string theme)))

(: log-styles-request (Boolean String String -> Void))
(define (log-styles-request offline? wikiname basename)
  (log "style" (offline-string offline?) wikiname basename))

(: log-set-settings-request (Symbol -> Void))
(define (log-set-settings-request theme)
  (log "settings" (symbol->string theme)))

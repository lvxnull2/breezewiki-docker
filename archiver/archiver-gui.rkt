#lang racket/base
(require racket/class
         racket/list
         racket/port
         racket/set
         racket/string
         db
         net/http-easy
         racket/gui/easy
         racket/gui/easy/operator
         "archiver-database.rkt"
         "archiver.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt")

(define active-threads (mutable-seteq))

(define/obs @auto-retry #f)
(define/obs @wikiname "")
(define/obs @state 'waiting)
(define/obs @num-pages 1)
(define/obs @done-pages 0)
(define/obs @just-done "")
(define/obs @queue '())
(define @title
  (obs-combine
   (λ (state queue num-pages done-pages)
     (define suffix (if (pair? queue)
                        (format " +~a" (length queue))
                        ""))
     (define progress (if (eq? num-pages 0)
                          " 0%"
                          (format " ~a%" (round (inexact->exact (* (/ done-pages num-pages) 100))))))
     (case state
       [(waiting stage-0) (format "Fandom Archiver~a" suffix)]
       [(stage-1) (format "Fandom Archiver 0%~a" suffix)]
       [(stage-2) (format "Fandom Archiver~a~a" progress suffix)]
       [(err) "ERROR Fandom Archiver"]
       [(done) "Fandom Archiver 100%"]))
   @state @queue @num-pages (obs-throttle @done-pages #:duration 5000)))

(define-syntax-rule (t body ...)
  (set-add! active-threads (thread (λ () body ...))))

(define (do-start-or-queue)
  (define wikiname (obs-peek @wikiname))
  (:= @wikiname "")
  (when (not (equal? (string-trim wikiname) ""))
    (@queue . <~ . (λ (q) (append q (list wikiname))))
    (shift-queue-maybe)))

(define (shift-queue-maybe)
  (when (memq (obs-peek @state) '(waiting done))
    (define q (obs-peek @queue))
    (cond
      [(pair? q)
       (define wikiname (car q))
       (:= @queue (cdr q))
       (do-start-stage1 wikiname)]
      [#t (:= @state 'done)])))

(define (do-start-stage1 wikiname)
  (:= @just-done "")
  (:= @done-pages 0)
  (:= @num-pages 1)
  (t (with-handlers ([exn:fail? (handle-graphical-exn wikiname)])
       (:= @state 'stage-0)
       (if-necessary-download-list-of-pages wikiname (λ (now-done num-pages just-done-name)
                                                       (:= @num-pages num-pages)
                                                       (:= @done-pages now-done)
                                                       (:= @just-done just-done-name)
                                                       (:= @state 'stage-1)))
       (do-start-stage2 wikiname))))

(define (do-start-stage2 wikiname)
  (:= @just-done "")
  (:= @num-pages 1)
  (:= @done-pages 0)
  (t (with-handlers ([exn:fail? (handle-graphical-exn wikiname)])
       (save-each-page wikiname (λ (now-done num-pages just-done-path)
                                  (:= @num-pages num-pages)
                                  (:= @done-pages now-done)
                                  (:= @just-done just-done-path)))
       (:= @state 'waiting)
       (shift-queue-maybe)))
  (:= @state 'stage-2))

(define (exn->string e)
  (with-output-to-string
    (λ ()
      (displayln (exn-message e))
      (displayln "context:")
      (for ([item (continuation-mark-set->context (exn-continuation-marks e))])
        (printf "  ~a" (srcloc->string (cdr item)))
        (when (car item)
          (printf ": ~a" (car item)))
        (displayln "")))))

(define ((handle-graphical-exn wikiname) e)
  (displayln (exn->string e) (current-error-port))
  (cond
    [(obs-peek @auto-retry)
     (do-retry-end wikiname)]
    [#t
     (:= @state 'err)
     (thread
      (λ ()
        (define/obs @visible? #t)
        (render
         (dialog #:title "Download Error"
                 #:style '(resize-border)
                 #:mixin (λ (%) (class % (super-new)
                                  (obs-observe! @visible? (λ (visible?) (send this show visible?)))))
                 (vpanel #:margin '(15 15)
                         (text "Encountered this error while downloading:")
                         (input #:style '(multiple hscroll)
                                #:min-size '(#f 200)
                                (exn->string e))
                         (button "Retry Now" (λ () (:= @visible? #f) (do-retry-now wikiname)))
                         (button "Retry Round-Robin" (λ () (:= @visible? #f) (do-retry-end wikiname)))
                         (button "Skip Wiki" (λ () (:= @visible? #f) (do-continue)))
                         (button "Use Auto-Retry" (λ ()
                                                    (:= @auto-retry #t)
                                                    (:= @visible? #f)
                                                    (do-retry-end wikiname)))
                         (text "Be careful not to auto-retry an infinite loop!")))
         main-window)))
     (sleep)
     ; make sure the old broken threads are all gone
     (for ([th active-threads]) (kill-thread th))
     (set-clear! active-threads)]))

(define (do-retry-now wikiname)
  (@queue . <~ . (λ (q) (append (list wikiname) q)))
  (:= @state 'waiting)
  (shift-queue-maybe))

(define (do-retry-end wikiname)
  (@queue . <~ . (λ (q) (append q (list wikiname))))
  (:= @state 'waiting)
  (shift-queue-maybe))

(define (do-continue)
  (:= @state 'waiting)
  (shift-queue-maybe))

(define (display-basename basename)
  (define limit 40)
  (cond [(string? basename)
         (define query (basename->name-for-query basename))
         (define segments (string-split query "/"))
         (when (and ((string-length query) . > . limit) ((length segments) . >= . 2))
           (set! query (string-append ".../" (last segments))))
         (when ((string-length query) . > . limit)
           (set! query (string-append (substring query 0 (- limit 3)) "...")))
         query]
        [#t "?"]))

(define main-window
  (render
   (window #:title @title
           #:size '(360 200)
           #:mixin (λ (%) (class %
                            (super-new)
                            (define/augment (on-close)
                              (for ([th active-threads]) (kill-thread th))
                              (disconnect slc))))
           ;; input box at the top
           (hpanel (text "https://")
                   (input @wikiname
                          (λ (event data) (cond
                                            [(eq? event 'input) (:= @wikiname data)]
                                            [(eq? event 'return) (do-start-or-queue)])))
                   (text ".fandom.com"))
           (button (@queue . ~> . (λ (q) (if (null? q) "Start" "Queue"))) (λ () (do-start-or-queue)))
           (text (@queue . ~> . (λ (q) (if (null? q) "" (string-join #:before-first "Queue: " q ", ")))))
           ;; show status based on overall application state
           (case-view
            @state
            ;; waiting for wikiname entry
            ((waiting) (vpanel
                        (text "Fill in the wikiname and click start.")))
            ((stage-0) (vpanel
                        (text "Checking data...")))
            ((stage-1) (vpanel
                        (text "Gathering list of pages...")
                        (text (@just-done . ~> . display-basename))
                        (text (@done-pages . ~> . (λ (x) (if (eq? x 0)
                                                             "0/?"
                                                             (format "~a/~a" x (obs-peek @num-pages))))))))
            ;; downloading contents
            ((stage-2) (vpanel
                        (text "Downloading page text...")
                        (progress @done-pages #:range @num-pages)
                        (text (@done-pages . ~> . (λ (x) (format "~a/~a" x (obs-peek @num-pages)))))
                        (text (@just-done . ~> . display-basename))))
            ((done) (vpanel
                     (text "All wikis downloaded!")))
            ((err) (vpanel
                      (text "Error. Check the popup window.")))
            (else (text (@state . ~> . (λ (state) (format "invalid state: ~a" state))))))
           (checkbox #:label "Auto-retry on error? (Dangerous)"
                     #:checked? @auto-retry
                     (λ:= @auto-retry)))))

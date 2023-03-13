#lang racket/base
(require racket/class
         racket/draw
         racket/format
         racket/list
         racket/port
         racket/set
         racket/splicing
         racket/string
         db
         net/http-easy
         memo
         (only-in racket/gui timer%)
         racket/gui/easy
         racket/gui/easy/operator
         (only-in pict bitmap)
         images/icons/arrow
         images/icons/control
         images/icons/stickman
         images/icons/style
         images/icons/symbol
         "archiver-database.rkt"
         "archiver.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt")

(default-icon-material rubber-icon-material)

(require (for-syntax racket/base racket/match racket/set racket/string))

(define-syntax (@> stx)
  (define form (cdr (syntax->datum stx)))
  (match form
    [(list form) ; (@> (fn @obs))
     ;; identify the observables and replace with non-@ symbols
     (define collection (mutable-set))
     (define updated
       (let loop ([sexp form])
         (cond [(symbol? sexp)
                (let ([as-s (symbol->string sexp)])
                  (if (string-prefix? as-s "@")
                      (let ([without-@ (string->symbol (substring as-s 1))])
                        (set-add! collection (cons sexp without-@))
                        without-@)
                      sexp))]
               [(pair? sexp) (cons (loop (car sexp)) (loop (cdr sexp)))]
               [#t sexp])))
     (define collection-l (set->list collection))
     ;; return obs-combine -> updated-form
     (datum->syntax stx `(obs-combine (λ (,@(map cdr collection-l)) ,updated) ,@(map car collection-l)))]
    [(list (? string? str) args ...) ; (@> "Blah: ~a/~a" @arg1 arg2)
     ;; identify the observables and replace with non-@ symbols
     (define collection-l
       (for/list ([arg args])
         (if (symbol? arg)
             (let ([as-s (symbol->string arg)])
               (if (string-prefix? as-s "@")
                   (let ([without-@ (string->symbol (substring as-s 1))])
                     (cons arg without-@))
                   (cons #f arg)))
             (cons #f arg))))
     (define collection-lo (filter car collection-l))
     ;; return obs-combine -> format
     (datum->syntax stx `(obs-combine (λ (,@(map cdr collection-lo)) (format ,str ,@(map cdr collection-l))) ,@(map car collection-lo)))]))

(define/obs @auto-retry #f)

(define-struct qi^ (wikiname st stage progress max-progress eta th) #:transparent) ;; queue item

(define rows (query-rows* "select wikiname, progress from wiki where progress < 4"))
(define/obs @queue null)
(define (add-wikiname-to-queue wikiname st stage)
  (@queue . <~ . (λ (queue)
                   (define already-exists? (findf (λ (qi) (equal? (qi^-wikiname qi) wikiname)) queue))
                   (if already-exists?
                       queue
                       (append queue (list (qi^ wikiname st stage 0 1 "..." #f)))))))
(for ([row rows])
  (add-wikiname-to-queue (vector-ref row 0)
                         (if (= (vector-ref row 1) 4)
                             'complete
                             'queued)
                         (vector-ref row 1)))

(define status-icon-size 32)
(define status-icon-min-width 36)
(define button-icon-size 12)

(define color-green (make-color 90 212 68))

(define/obs @input "")

(splicing-let ([frame-count 30])
  (define stickman-frames
    (for/vector ([s (in-range 0 1 (/ 1 frame-count))])
      (running-stickman-icon
       s
       #:height status-icon-size
       #:material (default-icon-material))))

  (define/obs @stick-frame-no 0)
  (define stick-timer
    (new timer%
         [notify-callback (λ () (@stick-frame-no . <~ . add1))]
         [interval (truncate (/ 1000 frame-count))]))
  (define/obs @stick
    (@stick-frame-no . ~> . (λ (n) (vector-ref stickman-frames
                                               (modulo n (vector-length stickman-frames)))))))

(define status-icons
  (hasheq 'queued (stop-icon #:color syntax-icon-color #:height status-icon-size)
          'paused (continue-forward-icon #:color syntax-icon-color #:height status-icon-size)
          'running @stick
          'error (x-icon #:height status-icon-size)
          'complete (check-icon #:color color-green #:height status-icon-size)))

(define action-icons
  (hasheq 'pause (pause-icon #:color syntax-icon-color #:height button-icon-size)
          'resume (play-icon #:color color-green #:height button-icon-size)
          'reset (left-over-arrow-icon #:color halt-icon-color #:height button-icon-size)))

(define (bitmap-view @the-bitmap [min-width 1])
  (pict-canvas #:min-size (@> (list (max min-width (send @the-bitmap get-width)) (send @the-bitmap get-height))) #;(if min-size (list min-size min-size) #f)
               #:stretch '(#f #f)
               #:style '(transparent)
               @the-bitmap
               bitmap))

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

(define ((handle-graphical-exn @qi) e)
  (displayln (exn->string e) (current-error-port))
  (cond
    [(obs-peek @auto-retry)
     (void) ;; TODO
     #;(do-retry-end wikiname)]
    [#t
     (update-qi @qi [st 'error])
     (do-try-unpause-next-entry)
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
                         ;; TODO
                         #;(button "Retry Now" (λ () (:= @visible? #f) (do-retry-now wikiname)))
                         #;(button "Retry Round-Robin" (λ () (:= @visible? #f) (do-retry-end wikiname)))
                         #;(button "Skip Wiki" (λ () (:= @visible? #f) (do-continue)))
                         #;(button "Use Auto-Retry" (λ ()
                                                      (:= @auto-retry #t)
                                                      (:= @visible? #f)
                                                      (do-retry-end wikiname)))
                         #;(text "Be careful not to auto-retry an infinite loop!")))
         main-window)))
     (sleep)
     ; make sure the broken thread is gone
     (define th (qi^-th (obs-peek @qi)))
     (when th (kill-thread th))]))

(define segments
  (list
   (list 5/100 (make-color 0 223 217))
   (list 88/100 color-green)
   (list 2/100 (make-color 0 223 217))
   (list 5/100 color-green)))
(define segment-spacing 2)
(unless (= (apply + (map car segments)) 1)
  (error 'segments "segments add up to ~a, not 1" (apply + (map car segments))))

;; return the new bitmap, which can be drawn on a dc<%>
(define/memoize (ray-trace width height stage progress max-progress)
  ;; (printf "rendering ~a ~a/~a at ~a~n" stage progress max-progress (current-inexact-milliseconds))
  (define bm (make-object bitmap% width height #f #t))
  (define dc (make-object bitmap-dc% bm))
  (define width-available (- width (* (length segments) segment-spacing)))
  (send dc set-smoothing 'unsmoothed)
  (send dc set-pen "black" 0 'transparent)
  (for/fold ([offset 0])
            ([segment segments]
             [i (in-naturals 0)]) ;; zero indexed stages?
    ;; calculate start and end locations of grey bar
    (define-values (segment-proportion segment-color) (apply values segment))
    (define segment-start (if (= offset 0) 0 (+ offset segment-spacing)))
    (define segment-width (* width-available segment-proportion))
    ;; draw grey bar
    (send dc set-brush (make-color 180 180 180 0.4) 'solid)
    (send dc draw-rectangle segment-start 0 segment-width height)
    ;; draw solid bar according to the current item's progress
    (define proportion
      (cond [(stage . < . i) 0]
            [(stage . > . i) 1]
            [(max-progress . <= . 0) 0]
            [(progress . < . 0) 0]
            [(progress . >= . max-progress) 1]
            [else (progress . / . max-progress)]))
    (send dc set-brush segment-color 'solid)
    (send dc draw-rectangle segment-start 0 (* proportion segment-width) height)
    (+ segment-start segment-width))
  (bitmap-render-icon bm 6/8))

;; get ray traced bitmap (possibly from cache) and draw on dc<%>
(define (draw-bar orig-dc qi)
  ;; (println ray-traced)
  (define-values (width height) (send orig-dc get-size))
  (send orig-dc draw-bitmap (ray-trace width height  (qi^-stage qi) (qi^-progress qi) (qi^-max-progress qi)) 0 0))

(define ((make-progress-updater @qi) a b c)
  ;; (printf "~a: ~a/~a ~a~n" (qi^-wikiname (obs-peek @qi)) a b c)
  (update-qi @qi [progress a] [max-progress b]))

(define (do-add-to-queue)
  (define wikiname (string-trim (obs-peek @input)))
  (when ((string-length wikiname) . > . 0)
    (add-wikiname-to-queue wikiname 'queued 0)) ;; TODO: automatically start?
  (:= @input ""))

(define-syntax-rule (update-qi @qi args ...)
  (let ([wikiname (qi^-wikiname (obs-peek @qi))])
    (@queue . <~ . (λ (queue)
                     (for/list ([qi queue])
                       (if (equal? (qi^-wikiname qi) wikiname)
                           (struct-copy qi^ qi args ...)
                           qi))))))

(define (do-start-qi @qi)
  (define th
    (thread (λ ()
              (with-handlers ([exn? (handle-graphical-exn @qi)])
                (define last-stage
                  (for/last ([stage all-stages]
                             [i (in-naturals)])
                    (update-qi @qi [stage i])
                    (stage (qi^-wikiname (obs-peek @qi)) (make-progress-updater @qi))
                    i))
                (update-qi @qi [st 'complete] [stage (add1 last-stage)])
                (do-try-unpause-next-entry)))))
  (update-qi @qi [st 'running] [th th]))

(define (do-stop-qi @qi)
  (define th (qi^-th (obs-peek @qi)))
  (when th (kill-thread th))
  (update-qi @qi [th #f] [st 'paused]))

(define (do-reset-qi @qi)
  (define th (qi^-th (obs-peek @qi)))
  (when th (kill-thread th))
  (update-qi @qi [th #f] [st 'queued] [stage 0] [progress 0] [max-progress 0])
  (query-exec* "update wiki set progress = 0 where wikiname = ?" (qi^-wikiname (obs-peek @qi))))

(define (do-try-unpause-next-entry)
  (define queue (obs-peek @queue))
  (define next-qi (for/first ([qi queue]
                              #:when (memq (qi^-st qi) '(paused queued error)))
                    qi))
  (when next-qi
    (define @qi (@queue . ~> . (λ (queue) (findf (λ (qi) (equal? (qi^-wikiname qi) (qi^-wikiname next-qi))) queue))))
    (do-start-qi @qi)))

(define main-window
  (render
   (window
    #:title "Fandom Archiver"
    #:size '(400 300)
    #:mixin (λ (%) (class %
                     (super-new)
                     (define/augment (on-close)
                       (send stick-timer stop)
                       (for ([qi (obs-peek @queue)])
                         (when (qi^-th qi)
                           (kill-thread (qi^-th qi))))
                       #;(disconnect*))))
    (vpanel
     #:spacing 10
     #:margin '(5 5)
     (hpanel
      #:stretch '(#t #f)
      #:spacing 10
      (hpanel
       (text "https://")
       (input @input
              (λ (event data) (cond
                                [(eq? event 'input) (:= @input data)]
                                [(eq? event 'return) (do-add-to-queue)])))
       (text ".fandom.com"))
      (button "Download Wiki" do-add-to-queue))
     (list-view
      #:style '(vertical)
      @queue
      #:key qi^-wikiname
      (λ (k @qi)
        (define @status-icons
          (@> (case (qi^-st @qi)
                [(running) @stick]
                [else (hash-ref status-icons (qi^-st @qi))])))
        (define @is-running?
          (@> (memq (qi^-st @qi) '(running))))
        (define @is-complete?
          (@> (eq? (qi^-st @qi) 'complete)))
        ;; state icon at the left side
        (hpanel #:stretch '(#t #f)
                #:alignment '(left center)
                #:spacing 8
                (bitmap-view @status-icons status-icon-min-width)
                (vpanel
                 ;; name and buttons (top half)
                 (hpanel #:alignment '(left bottom)
                         (text (@> (qi^-wikiname @qi)))
                         (spacer)
                         (hpanel
                          #:stretch '(#f #f)
                          (if-view @is-complete?
                                   (button (hash-ref action-icons 'reset)
                                           (λ () (do-reset-qi @qi)))
                                   (spacer))
                          (if-view @is-running?
                                   (button (hash-ref action-icons 'pause)
                                           (λ () (do-stop-qi @qi)))
                                   (button (hash-ref action-icons 'resume)
                                           (λ () (do-start-qi @qi))))))
                 ;; progress bar (bottom half)
                 (hpanel
                  (canvas
                   @qi
                   #:style '(transparent)
                   #:margin '(3 3)
                   draw-bar)
                  (hpanel #:min-size '(68 #f)
                          #:stretch '(#f #f)
                          #:alignment '(right center)
                          (text (@> (format "eta ~a" (qi^-eta @qi))))))))))))))

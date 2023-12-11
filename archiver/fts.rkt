#lang cli
(require (for-syntax racket/base))
(require racket/format
         racket/function
         racket/future
         racket/match
         racket/path
         racket/promise
         racket/port
         racket/runtime-path
         racket/string
         file/gunzip
         db
         db/unsafe/sqlite3
         net/http-easy
         json
         json-pointer
         "../lib/html-parsing/main.rkt"
         "../lib/xexpr-utils.rkt"
         "../lib/tree-updater.rkt")

(flag (read-from-cache?)
      ("-c" "--read-from-cache" "read from last run cache instead of rebuilding documents")
      (read-from-cache? #t))

(define-runtime-path storage-path "../storage/archive")

;; ***************************************************************************************************
;; Progress bar display
;; ***************************************************************************************************

(struct progress^ (n max title) #:transparent)

(define (make-m-s seconds)
  (define-values (eta-m eta-s) (quotient/remainder seconds 60))
  (format "~a:~a" eta-m (~a eta-s #:width 2 #:align 'right #:pad-string "0")))

(define (make-progress get-p [history-size 20])
  (define update-sleep 1)
  (define name-width 30)
  (define max-width 105)
  (define history (make-vector history-size 0))
  (define history-pointer 0)
  (define elapsed 0)
  (define (report-progress)
    (define p (get-p))
    (define history-cycle (vector-ref history history-pointer))
    (vector-set! history history-pointer (progress^-n p))
    (set! history-pointer (modulo (add1 history-pointer) history-size))
    (set! elapsed (add1 elapsed))
    (define-values (eta-display diff-per-second)
      (cond
        [((progress^-n p) . >= . (progress^-max p)) (values (format "~a **" (make-m-s elapsed)) (format "** ~a" (quotient (progress^-max p) (max elapsed 1))))]
        [(= history-cycle 0) (values "-:--" "--")]
        [else (define diff-per-second (/ (- (progress^-n p) history-cycle) (* history-size update-sleep)))
              (define eta-total
                (if (diff-per-second . > . 0)
                    (floor (round (/ (- (progress^-max p) (progress^-n p)) diff-per-second)))
                    0))
              (values (make-m-s eta-total)
                      (round diff-per-second))]))
    (define left (format "~a/~a ~a/s ~a ~a%"
                         (~a (progress^-n p) #:width (string-length (~a (progress^-max p))) #:align 'right #:pad-string " ")
                         (progress^-max p)
                         diff-per-second
                         eta-display
                         (floor (* 100 (/ (progress^-n p) (progress^-max p))))))
    (define name-display (~a (progress^-title p) #:max-width name-width #:limit-marker "..."))
    (define remaining-space (- max-width name-width (string-length left) 2))
    (define bar-width
      (floor (* (sub1 remaining-space)
                (/ (progress^-n p) (progress^-max p)))))
    (define bar (string-append (make-string bar-width #\=)
                               ">"
                               (make-string (- remaining-space bar-width) #\ )))
    (printf "\e[2K\r~a~a~a" left bar name-display)
    (flush-output))
  (define (report-progress-loop)
    (sleep update-sleep)
    (report-progress)
    (report-progress-loop))
  (define t (thread report-progress-loop))
  (define (quit)
    (kill-thread t)
    (report-progress)
    (displayln ""))
  quit)

;; ***************************************************************************************************
;; Page text extractor
;; ***************************************************************************************************

(define (class-has? attributes substrs)
  (define cl (or (get-attribute 'class attributes) ""))
  (ormap (λ (substr) (string-contains? cl substr)) substrs))

(define (updater element element-type attributes children)
  (cond
    [(class-has? attributes '("collapsed" "selflink" "label" "toc" "editsection" "reviews"))
     (list 'div '() '())]
    [#t
     (list element-type attributes children)]))

(define (writer tables-mode? page)
  (define (writer-inner page)
    (for ([bit page])
      (cond
        [(and tables-mode? (pair? bit) (memq (car bit) '(h1 h2 h3 p blockquote q))) (void)]
        [(and (not tables-mode?) (pair? bit) (memq (car bit) '(ul ol dl table))) (void)]
        [(memq bit '(div p li td dd dt br)) (displayln "")]
        [(symbol? bit) (void)]
        [(and (pair? bit) (eq? (car bit) '*COMMENT*)) (void)]
        [(and (pair? bit) (eq? (car bit) '@)) (void)]
        [(pair? bit) (writer-inner bit)]
        [(string? bit) (display bit)])))
  (writer-inner page))

(define (write-and-post-process tables-mode? page)
  (define text (with-output-to-string (λ () (writer tables-mode? page))))
  ;; (define text-no-numbers (regexp-replace* #px"(?:-|[+$£€¥] *)?[0-9,.]{2,}%?\\s*" text ""))
  (define shrink-text (regexp-replace* #px"([ \t]*\r?\n+)+" text "\n"))
  shrink-text)

(define ((extract f)) ; f - filename
  (with-handlers
    ([exn:fail? (λ (err) (printf "extract: ~a: ~v~n" f err))])
    (define j
      (case (path-get-extension f)
        [(#".json")
         (with-input-from-file f (λ () (read-json)))]
        [(#".gz")
         (define-values (in out) (make-pipe))
         (with-input-from-file f (λ () (gunzip-through-ports (current-input-port) out)))
         (read-json in)]
        [else #f]))
    (define title (json-pointer-value "/parse/title" j))
    (define pageid (json-pointer-value "/parse/pageid" j))
    (define page-html (preprocess-html-wiki (json-pointer-value "/parse/text" j)))
    (define page (update-tree updater (html->xexp page-html)))
    (define body (write-and-post-process #f page))
    (define table (write-and-post-process #t page))
    (list title body table pageid)))

;; ***************************************************************************************************
;; Program, loop, Solr APIs
;; ***************************************************************************************************

(program
 (start [wikiname "wikiname to download"])

 (define results
  (for/list ([f (directory-list (build-path storage-path wikiname) #:build? #t)]
             #:when (member (path-get-extension f) '(#".gz")))
    (extract f)))

 (define data
   (cond
     [(and (read-from-cache?) (file-exists? "cache.rkt"))
        (displayln "Reading in...")
        (with-input-from-file "cache.rkt" (λ () (read)))]
     [else
      (define x (box (progress^ 0 1 "...")))
      (define quit (make-progress (λ () (unbox x))))
      (define data
        (for/list ([fut results]
                   [i (in-naturals 1)]
                   #:do [(define page (fut))]
                   #:when (not (void? page)))
          (match-define (list title body table pageid) page)
          (define len (string-length body))
          (set-box! x (progress^ i (length results) title))
          `#hasheq((id . ,(number->string pageid))
                   (title . ,title)
                   (body . ,body)
                   (table . ,table)
                   (len . ,len))))
      (quit)

      (display "Writing out... ")
      (flush-output)
      (with-output-to-file "cache.rkt" (λ () (write data)) #:exists 'truncate/replace)
      data]))

 (display "Converting... ")
 (flush-output)
 (define ser (jsexpr->bytes data))
 (define ser-port (open-input-bytes ser))
 (define quit (make-progress (λ () (progress^ (ceiling (/ (file-position ser-port) 64 1024))
                                              (ceiling (/ (bytes-length ser) 64 1024))
                                              "Posting..."))
                             2))
 (define res
   (post (format "http://localhost:8983/solr/~a/update?commit=true" wikiname)
         #:data ser-port
         #:headers '#hasheq((Content-Type . "application/json"))
         #:timeouts (make-timeout-config #:lease 5 #:connect 5 #:request 300)))
 (quit)
 (displayln (response-status-line res)))

(run start)

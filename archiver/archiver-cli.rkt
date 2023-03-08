#lang cli
(require charterm
         "archiver.rkt")

(help (usage "Downloads a single Fandom wiki in BreezeWiki offline format."
             ""
             "Downloaded pages go into `archive/` next to the executable."
             "Database goes into `archiver.db*` next to the executable."
             "The database is necessary to store your download progress and resume where you left off if the process is interrupted."))

(flag (output-quiet?)
      ("-q" "--output-quiet" "disable progress output")
      (output-quiet? #t))

(flag (output-progress?)
      ("-p" "--output-progress" "progress output for terminals (default in a tty)")
      (output-progress? #t))

(flag (output-lines?)
      ("-l" "--output-lines" "output the name of each file downloaded (default outside of a tty)")
      (output-lines? #t))

(constraint (one-of output-quiet? output-lines? output-progress?))



(program
 (start [wikiname "wikiname to download"])
 ;; set up arguments
 (define width 80)
 (when (not (or (output-quiet?) (output-lines?) (output-progress?)))
   (cond [(terminal-port? current-input-port)
          (output-progress? #t)]
         [else
          (output-lines? #t)]))
 (define (update-width)
   (when (output-progress?)
     (with-charterm
       (call-with-values (λ () (charterm-screen-size))
                         (λ (cols rows) (set! width cols))))))
 (update-width)
 ;; check
 (when (or (not wikiname) (equal? wikiname ""))
   (raise-user-error "Please specify the wikiname to download on the command line."))
 ;; progress reporting based on selected mode
 (define (report-progress a b c)
   (define basename (basename->name-for-query c))
   (cond
     [(output-lines?)
      (displayln basename)]
     [(output-progress?)
      (when (eq? (modulo a 20) 0)
        (thread (λ () (update-width))))
      (define prefix (format "[~a] [~a/~a] " wikiname a b))
      (define rest (- width (string-length prefix)))
      (define real-width (min (string-length basename) rest))
      (define spare-width (- rest real-width))
      (define name-display (substring basename 0 real-width))
      (define whitespace (make-string spare-width #\ ))
      (printf "~a~a~a\r" prefix name-display whitespace)]))
 ;; download all stages
 (for ([stage all-stages]
       [i (in-naturals 1)])
   (printf "> Stage ~a/~a~n" i (length all-stages))
   (stage wikiname report-progress)
   (displayln "")))

(run start)

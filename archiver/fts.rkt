#lang racket
(require racket/function
         racket/future
         racket/match
         racket/path
         racket/promise
         racket/port
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

(define-syntax (seq stx)
  (syntax-case stx ()
    [(_ body ...)
     #`(for ([op (list (lambda () body) ...)]
             [i (in-naturals)])
         (define res (op))
         (when (>= (response-status-code res) 400)
           (error 'seq "op #~a: status code was ~a: ~v" i (response-status-code res) (response-json res)))
         (define taskuid (json-pointer-value "/taskUid" (response-json res)))
         (for/or ([ticks (in-naturals)]
                  [res2 (in-producer (lambda () (get (format "http://localhost:7700/tasks/~a" taskuid))))])
           (define status (json-pointer-value "/status" (response-json res2)))
           (case status
             [("enqueued" "processing")
              (sleep 1)
              #f]
             [("succeeded")
              (printf "op #~a: ~a (~a ticks)~n" i status ticks)
              #t]
             [else
              (error 'seq "op #~a: task status was ~a: ~v" i status res2)])))]))

(define (class-has? attributes substrs)
  (define cl (or (get-attribute 'class attributes) ""))
  (ormap (λ (substr) (string-contains? cl substr)) substrs))

(define (updater element element-type attributes children)
  (cond
    [(class-has? attributes '("collapsed" "selflink" "label" "toc" "editsection" "reviews"))
     (list 'div '() '())]
    [#t
     (list element-type attributes children)]))

(define slc (sqlite3-connect #:database "../storage/fts-separate.db"))
(sqlite3-load-extension slc "fts5")

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

(define wikiname "bloons")
(define tablename (format "page_~a" wikiname))

(define ((extract f)) ; f - filename
  (with-handlers
    ([exn:fail? (λ (err) (println f) (raise err))])
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
    (values title body table pageid)))

(define results
  (for/list ([f (directory-list (format "../storage/archive/~a" wikiname) #:build? #t)]
             #:when (member (path-get-extension f) '(#".json" #".gz")))
    (extract f)))

;; ***************************************************************************************************
;; TESTING WRITER
;; ***************************************************************************************************
#;(for/first ([fut results]
            [i (in-naturals 1)]
            #:when (i . >= . 4859))
  (define-values (title body table pageid) (fut))
  (println title)
  (println body)
  (println table))

(println "inserting...")

;; ***************************************************************************************************
;; SQLite FTS5
;; ***************************************************************************************************
#;(begin
    (query-exec slc "begin transaction")
    #;(query-exec slc (format "create virtual table \"~a\" using fts5 (title, body, tokenize='porter unicode61')" wikiname))
    (time
     (for ([fut results]
           [i (in-naturals 1)])
       (display "-")
       (when (and (> i 0) (= (modulo i 100) 0))
         (println i))
       (define-values (title shrink-text) (fut))
       (query-exec slc (format "insert into \"~a\" (title, body) values (?, ?)" tablename) title shrink-text)))

    (println "running optimize...")
    (query-exec slc (format "insert into \"~a\" (\"~a\") values ('optimize')" tablename tablename))

    (println "committing...")
    (query-exec slc "commit"))

;; ***************************************************************************************************
;; Solr
;; ***************************************************************************************************
(begin
  (define data
    (cond
      #;[(file-exists? "cache.rkt")
       (println "reading in...")
       (with-input-from-file "cache.rkt" (λ () (read)))]
      [else
       (define data
         (for/list ([fut results]
                    [i (in-naturals 1)])
           (display "-")
           (when (and (> i 0) (= (modulo i 100) 0))
             (println i))
           (define-values (title body table pageid) (fut))
           (define len (string-length body))
           `#hasheq((id . ,(number->string pageid))
                    (title . ,title)
                    (body . ,body)
                    (table . ,table)
                    (len . ,len))))

       (println "writing out...")
       (with-output-to-file "cache.rkt" (λ () (write data)) #:exists 'truncate/replace)
       data]))

  (println "posting...")
  (define res
    (post (format "http://localhost:8983/solr/~a/update?commit=true" wikiname)
          #:json data)))

;; ***************************************************************************************************
;; Meilisearch
;; ***************************************************************************************************
#;(begin
  (seq
   (put (format "http://localhost:7700/indexes/~a/settings/searchable-attributes" wikiname)
        #:json '("title" "body"))
   (put (format "http://localhost:7700/indexes/~a/settings/ranking-rules" wikiname)
        #:json '("words" "typo" #;"proximity" "attribute" "sort" "exactness" #;"len:desc"))
   (call-with-input-file "stop-words.json"
     (λ (in)
       (put (format "http://localhost:7700/indexes/~a/settings/stop-words" wikiname)
            #:headers '#hasheq((Content-Type . "application/json"))
            #:data in))))
  (define data
    (for/list ([fut results]
               [i (in-naturals 1)])
      (display "-")
      (when (and (> i 0) (= (modulo i 100) 0))
        (println i))
      (define-values (title body pageid) (fut))
      (define len (string-length body))
      `#hasheq((id . ,pageid)
               (title . ,title)
               (body . ,body)
               (len . ,len))))
  (define res
    (post (format "http://localhost:7700/indexes/~a/documents" wikiname)
          #:json data))
  (seq res)
  (println (response-json res)))

(disconnect slc)

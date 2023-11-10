#lang racket/base
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
         json
         json-pointer
         "../lib/html-parsing/main.rkt"
         "../lib/xexpr-utils.rkt"
         "../lib/tree-updater.rkt")

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

(define (writer page)
  (for ([bit page])
    (cond
      [(memq bit '(div p li td)) (displayln "")]
      [(symbol? bit) (void)]
      [(and (pair? bit) (eq? (car bit) '*COMMENT*)) (void)]
      [(and (pair? bit) (eq? (car bit) '@)) (void)]
      [(pair? bit) (writer bit)]
      [(string? bit) (display bit)])))

(define wikiname "sto")
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
    (define page-html (preprocess-html-wiki (json-pointer-value "/parse/text" j)))
    (define page (update-tree updater (html->xexp page-html)))
    (define text (with-output-to-string (λ () (writer page))))
    (define shrink-text (regexp-replace* #px"([ \t]*\r?\n+)+" text "\n"))
    (values title shrink-text)))

(println "extracting text...")
(define results
  (time
   (for/list ([f (directory-list (format "../storage/archive/~a" wikiname) #:build? #t)]
              #:when (member (path-get-extension f) '(#".json" #".gz")))
     (extract f))))

(println "inserting...")
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
(query-exec slc "commit")

(disconnect slc)

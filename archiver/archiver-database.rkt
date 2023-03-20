#lang racket/base
(require racket/file
         racket/list
         racket/path
         racket/string
         json
         json-pointer
         db
         "../lib/syntax.rkt")

(provide
 get-slc
 query-exec*
 query-rows*
 query-list*
 query-value*
 query-maybe-value*
 query-maybe-row*)

(define storage-path (anytime-path ".." "storage"))
(define database-file (build-path storage-path "archiver.db"))

(define slc (box #f))
(define (get-slc)
  (define slc* (unbox slc))
  (cond
    [slc* slc*]
    [else
     (make-directory* storage-path)
     (define slc* (sqlite3-connect #:database database-file #:mode 'create))
     (query-exec slc* "PRAGMA journal_mode=WAL")
     (define database-version
       (with-handlers ([exn:fail:sql?
                        (λ (exn)
                          ; need to set up the database
                          (query-exec slc* "create table database_version (version integer, primary key (version))")
                          (query-exec slc* "insert into database_version values (0)")
                          0)])
         (query-value slc* "select version from database_version")))

     (define migrations
       (wrap-sql
        ((query-exec slc* "create table page (wikiname TEXT NOT NULL, basename TEXT NOT NULL, progress INTEGER NOT NULL, PRIMARY KEY (wikiname, basename))")
         (query-exec slc* "create table wiki (wikiname TEXT NOT NULL, progress INTEGER, PRIMARY KEY (wikiname))"))
        ((query-exec slc* "create table special_page (wikiname TEXT NOT NULL, key TEXT NOT NULL, basename TEXT NOT NULL, PRIMARY KEY (wikiname, key))"))
        ((query-exec slc* "update wiki set progress = 2 where wikiname in (select wikiname from wiki inner join page using (wikiname) group by wikiname having min(page.progress) = 1)"))
        ((query-exec slc* "create table image (wikiname TEXT NOT NULL, hash TEXT NTO NULL, url TEXT NOT NULL, ext TEXT, source INTEGER NOT NULL, progress INTEGER NOT NULL, PRIMARY KEY (wikiname, hash))"))
        ((query-exec slc* "alter table wiki add column sitename TEXT")
         (query-exec slc* "alter table wiki add column basepage TEXT")
         (query-exec slc* "alter table wiki add column license_text TEXT")
         (query-exec slc* "alter table wiki add column license_url TEXT"))))

     (let do-migrate-step ()
       (when (database-version . < . (length migrations))
         (call-with-transaction
          slc*
          (list-ref migrations database-version))
         (set! database-version (add1 database-version))
         (query-exec slc* "update database_version set version = $1" database-version)
         (do-migrate-step)))

     (set-box! slc slc*)
     slc*]))

(define (query-exec* . args)
  (apply query-exec (get-slc) args))

(define (query-rows* . args)
  (apply query-rows (get-slc) args))

(define (query-list* . args)
  (apply query-list (get-slc) args))

(define (query-value* . args)
  (apply query-value (get-slc) args))

(define (query-maybe-value* . args)
  (apply query-maybe-value (get-slc) args))

(define (query-maybe-row* . args)
  (apply query-maybe-row (get-slc) args))

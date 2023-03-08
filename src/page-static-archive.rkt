#lang racket/base
(require racket/file
         racket/path
         racket/port
         racket/string
         net/url
         web-server/http
         web-server/servlet-dispatch
         web-server/dispatchers/filesystem-map
         (only-in web-server/dispatchers/dispatch next-dispatcher)
         "../archiver/archiver.rkt"
         "../lib/mime-types.rkt"
         "../lib/syntax.rkt"
         "../lib/xexpr-utils.rkt"
         "config.rkt"
         "log.rkt")

(provide
 page-static-archive)

(define path-archive (anytime-path ".." "storage/archive"))

(define ((replacer wikiname) whole url)
  (format
   "url(~a)"
   (if (or (equal? url "")
           (equal? url "'")
           (string-contains? url "/resources-ucp/")
           (string-contains? url "/fonts/")
           (string-contains? url "/drm_fonts/")
           (string-contains? url "//db.onlinewebfonts.com/")
           (string-contains? url "//bits.wikimedia.org/")
           (string-contains? url "dropbox")
           (string-contains? url "only=styles")
           (string-contains? url "https://https://")
           (regexp-match? #rx"^%20|^'" url)
           (regexp-match? #rx"^\"?data:" url))
       url
       (let* ([norm-url
               (cond
                 [(string-prefix? url "https://") url]
                 [(string-prefix? url "http://") (regexp-replace #rx"http:" url "https:")]
                 [(string-prefix? url "//") (string-append "https:" url)]
                 [(string-prefix? url "/") (format "https://~a.fandom.com~a" wikiname url)]
                 [else (error 'replace-style-for-images "unknown URL format: ~a" url)])])
         (define p (image-url->values norm-url))
         ;; (printf "hashed: ~a~n     -> ~a~n    #-> ~a~n" url (car p) (cdr p))
         (format "/archive/~a/images/~a" wikiname (cdr p))))))

(define (replace-style-for-images wikiname path)
  (define content (file->string path))
  (regexp-replace* #rx"url\\(\"?'?([^)]*)'?\"?\\)" content (replacer wikiname)))

(define (handle-style wikiname dest)
  (when (config-true? 'debug)
    (printf "using offline mode for style ~a ~a~n" wikiname dest))
  (log-styles-request #t wikiname dest)
  (define fs-path (build-path path-archive wikiname "styles" dest))
  (unless (file-exists? fs-path)
    (next-dispatcher))
  (response-handler
   (define new-content (replace-style-for-images wikiname fs-path))
   (response/output
    #:code 200
    #:headers (list (header #"Content-Type" #"text/css")
                    (header #"Referrer-Policy" #"same-origin"))
    (λ (out) (displayln new-content out)))))

(define (handle-image wikiname dest) ;; dest is the hash with no extension
  (unless ((string-length dest) . >= . 40) (next-dispatcher))
  (response-handler
   (define dir (build-path path-archive wikiname "images" (substring dest 0 1) (substring dest 0 2)))
   (unless (directory-exists? dir) (next-dispatcher))
   (define candidates (directory-list dir))
   (define target (path->string (findf (λ (f) (string-prefix? (path->string f) dest)) candidates)))
   (unless target (next-dispatcher))
   (define ext (substring target 41))
   (response/output
    #:code 200
    #:headers (list (header #"Content-Type" (ext->mime-type (string->bytes/latin-1 ext))))
    (λ (out)
      (call-with-input-file (build-path dir target)
        (λ (in)
          (copy-port in out)))))))

(define (page-static-archive req)
  (define path (url-path (request-uri req)))
  (define-values (_ wikiname kind dest) (apply values (map path/param-path path)))
  (cond [(equal? kind "styles") (handle-style wikiname dest)]
        [(equal? kind "images") (handle-image wikiname dest)]
        [else (response-handler (raise-user-error "page-static-archive: how did we get here?" kind))]))

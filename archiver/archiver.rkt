#lang racket/base
(require racket/file
         racket/function
         racket/list
         racket/runtime-path
         racket/string
         net/url
         net/mime
         file/sha1
         net/http-easy
         db
         "../lib/html-parsing/main.rkt"
         json
         "archiver-database.rkt"
         "../lib/mime-types.rkt"
         "../lib/tree-updater.rkt"
         "../lib/url-utils.rkt"
         "../lib/xexpr-utils.rkt"
         "../lib/archive-file-mappings.rkt")

(define archive-slc slc)

(provide
 if-necessary-download-list-of-pages
 download-list-of-pages
 save-each-page
 basename->name-for-query
 image-url->values
 hash->save-dir
 archive-slc)

(module+ test
  (require rackunit))

(define-runtime-path archive-root "../storage/archive")
#;(define archive-root "archive")

(define sources '#hasheq((style . 1) (page . 2)))

(define (get-origin wikiname)
  (format "https://~a.fandom.com" wikiname))

(define (insert-wiki-entry wikiname)
  (define dest-url
    (format "https://~a.fandom.com/api.php?~a"
            wikiname
            (params->query '(("action" . "query")
                             ("meta" . "siteinfo")
                             ("siprop" . "general|rightsinfo")
                             ("format" . "json")
                             ("formatversion" . "2")))))
  (define data (response-json (get dest-url)))
  (define exists? (query-maybe-value slc "select progress from wiki where wikiname = ?" wikiname))
  (if exists?
      (query-exec slc "update wiki set sitename = ?, basepage = ?, license_text = ?, license_url = ? where wikiname = ?"
                  (jp "/query/general/sitename" data)
                  (second (regexp-match #rx"/wiki/(.*)" (jp "/query/general/base" data)))
                  (jp "/query/rightsinfo/text" data)
                  (jp "/query/rightsinfo/url" data)
                  wikiname)
      (query-exec slc "insert into wiki (wikiname, progress, sitename, basepage, license_text, license_url) values (?, 1, ?, ?, ?, ?)"
                  wikiname
                  (jp "/query/general/sitename" data)
                  (second (regexp-match #rx"/wiki/(.*)" (jp "/query/general/base" data)))
                  (jp "/query/rightsinfo/text" data)
                  (jp "/query/rightsinfo/url" data))))

;; call 1 if not yet done for that wiki
(define (if-necessary-download-list-of-pages wikiname callback)
  (define wiki-progress (query-maybe-value slc "select progress from wiki where wikiname = ?" wikiname))
  ;; done yet?
  (unless (and (real? wiki-progress) (wiki-progress . >= . 1))
    ;; count total pages
    (define dest-url
      (format "https://~a.fandom.com/api.php?~a"
              wikiname
              (params->query `(("action" . "query") ("meta" . "siteinfo") ("siprop" . "statistics") ("format" . "json")))))
    (define num-pages (jp "/query/statistics/articles" (response-json (get dest-url))))
    (download-list-of-pages wikiname callback 0 num-pages #f)))

;; 1. Download list of wiki pages and store in database
(define (download-list-of-pages wikiname callback total-so-far grand-total path-with-namefrom)
  (define url (if path-with-namefrom
                  (format "https://~a.fandom.com~a" wikiname path-with-namefrom)
                  (format "https://~a.fandom.com/wiki/Local_Sitemap" wikiname)))
  (define r (get url))
  (define page (html->xexp (bytes->string/utf-8 (response-body r))))
  (define link-namefrom
    ((query-selector (λ (t a c x) (and (eq? t 'a)
                                       (pair? x)
                                       (string-contains? (car x) "Next page")
                                       (let ([href (get-attribute 'href a)] )
                                         (and href (string-contains? href "/wiki/Local_Sitemap")))))
                     page #:include-text? #t)))
  (define row-values
    (for/list ([link (in-producer
                      (query-selector
                       (λ (t a c) (eq? t 'a))
                       ((query-selector (λ (t a c) (has-class? "mw-allpages-chunk" a)) page)))
                      #f)])
      (list wikiname (local-encoded-url->basename (get-attribute 'href (bits->attributes link))) 0)))
  (define query-template (string-join (make-list (length row-values) "(?, ?, ?)") ", " #:before-first "insert or ignore into page (wikiname, basename, progress) values "))
  (apply query-exec slc query-template (flatten row-values))
  (define new-total (+ (length row-values) total-so-far))
  (callback new-total grand-total (second (last row-values)))
  (cond
    [link-namefrom ; repeat on the next page
     (download-list-of-pages wikiname callback new-total grand-total (get-attribute 'href (bits->attributes link-namefrom)))]
    [#t ; all done downloading sitemap
     (insert-wiki-entry wikiname)]))

;; 2. Download each page via API and:
;; * Save API response to file
(define max-page-progress 1)
(define (save-each-page wikiname callback)
  ;; prepare destination folder
  (define save-dir (build-path archive-root wikiname))
  (make-directory* save-dir)
  ;; gather list of basenames to download (that aren't yet complete)
  (define basenames (query-list slc "select basename from page where wikiname = ? and progress < ?"
                                wikiname max-page-progress))
  ;; counter of complete/incomplete basenames
  (define already-done-count
    (query-value slc "select count(*) from page where wikiname = ? and progress = ?"
                 wikiname max-page-progress))
  (define not-done-count
    (query-value slc "select count(*) from page where wikiname = ? and progress < ?"
                 wikiname max-page-progress))
  ;; set initial progress
  (callback already-done-count (+ already-done-count not-done-count) "")
  ;; loop through basenames and download
  (for ([basename basenames]
        [i (in-naturals 1)])
    (define name-for-query (basename->name-for-query basename))
    (define dest-url
      (format "https://~a.fandom.com/api.php?~a"
              wikiname
              (params->query `(("action" . "parse")
                               ("page" . ,name-for-query)
                               ("prop" . "text|headhtml|langlinks")
                               ("formatversion" . "2")
                               ("format" . "json")))))
    (define r (get dest-url))
    (define body (response-body r))
    (define filename (string-append basename ".json"))
    (define save-path
      (cond [((string-length basename) . > . 240)
             (define key (sha1 (string->bytes/latin-1 basename)))
             (query-exec slc "insert into special_page (wikiname, key, basename) values (?, ?, ?)"
                         wikiname key basename)
             (build-path save-dir (string-append key ".json"))]
            [#t
             (build-path save-dir (string-append basename ".json"))]))
    (display-to-file body save-path #:exists 'replace)
    (query-exec slc "update page set progress = 1 where wikiname = ? and basename = ?"
                wikiname basename)
    (callback (+ already-done-count i) (+ already-done-count not-done-count) basename))
  ;; saved all pages, register that fact in the database
  (query-exec slc "update wiki set progress = 2 where wikiname = ?" wikiname))

;; 3. Download CSS and:
;; * Save CSS to file
;; * Record style images to database
(define (check-style-for-images wikiname path)
  (define content (file->string path))
  (define urls (regexp-match* #rx"url\\(\"?'?([^)]*)'?\"?\\)" content #:match-select cadr))
  (for/list ([url urls]
             #:when (not (or (equal? url "")
                             (equal? url "'")
                             (string-contains? url "/resources-ucp/")
                             (string-contains? url "/fonts/")
                             (string-contains? url "/drm_fonts/")
                             (string-contains? url "//db.onlinewebfonts.com/")
                             (string-contains? url "//bits.wikimedia.org/")
                             (string-contains? url "dropbox")
                             (string-contains? url "only=styles")
                             (string-contains? url "https://https://")
                             (regexp-match? #rx"^%20" url)
                             (regexp-match? #rx"^data:" url))))
    (cond
      [(string-prefix? url "https://") url]
      [(string-prefix? url "http://") (regexp-replace #rx"http:" url "https:")]
      [(string-prefix? url "//") (string-append "https:" url)]
      [(string-prefix? url "/") (format "https://~a.fandom.com~a" wikiname url)]
      [else (raise-user-error "While calling check-style-for-images, this URL had an unknown format and couldn't be saved:" url path)])))

(define (download-styles-for-wiki wikiname)
  (define save-dir (build-path archive-root wikiname "styles"))
  (make-directory* save-dir)
  (define theme (λ (theme-name)
                  (cons (format "https://~a.fandom.com/wikia.php?controller=ThemeApi&method=themeVariables&variant=~a" wikiname theme-name)
                        (build-path save-dir (format "themeVariables-~a.css" theme-name)))))
  ;; (Listof (Pair url save-path))
  (define styles
    (list
     (theme "default")
     (theme "light")
     (theme "dark")
     (cons (format "https://~a.fandom.com/load.php?lang=en&modules=skin.fandomdesktop.styles%7Cext.fandom.PortableInfoboxFandomDesktop.css%7Cext.fandom.GlobalComponents.CommunityHeaderBackground.css%7Cext.gadget.site-styles%2Csound-styles%7Csite.styles&only=styles&skin=fandomdesktop" wikiname)
           (build-path save-dir "site.css"))))
  (for ([style styles])
    (define r (get (car style)))
    (define body (response-body r))
    (display-to-file body (cdr style) #:exists 'replace)
    ;; XXX: how the HELL do I deal with @import?? would need some kind of recursion here. how will the page server know where to look up the style file to be able to serve them again? do I add another link-stylesheet tag to the main page? what about the remaining stuck @import url?
    )
  styles)

(define (do-step-3 wikiname)
  (define wiki-progress (query-maybe-value slc "select progress from wiki where wikiname = ?" wikiname))
  (unless (and (number? wiki-progress) (wiki-progress . >= . 3))
    (define styles (download-styles-for-wiki wikiname))
    (define unique-image-urls
      (remove-duplicates
       (map image-url->values
            (flatten
             (for/list ([style styles])
               (check-style-for-images wikiname (cdr style)))))
       #:key cdr))
    (println unique-image-urls)
    (for ([pair unique-image-urls])
      (query-exec slc "insert or ignore into image (wikiname, url, hash, ext, source, progress) values (?, ?, ?, NULL, 1, 0)" wikiname (car pair) (cdr pair)))
    (query-exec slc "update wiki set progress = 3 where wikiname = ?" wikiname)))

;; 4: From downloaded pages, record URLs of image sources and inline style images to database
(define (hash->save-dir wikiname hash)
  (build-path archive-root wikiname "images" (substring hash 0 1) (substring hash 0 2)))

(define (image-url->values i)
  ;; TODO: handle case where there is multiple cb parameter on minecraft wiki
  ;; TODO: ensure it still "works" with broken &amp; on minecraft wiki
  (define no-cb (regexp-replace #rx"\\cb=[0-9]+&?" i "")) ; remove cb url parameter which does nothing
  (define key (regexp-replace #rx"[&?]$" no-cb "")) ; remove extra separator if necessary
  (define hash (sha1 (string->bytes/utf-8 key)))
  (cons key hash))

(define (check-json-for-images wikiname path)
  (define data (with-input-from-file path (λ () (read-json))))
  (define page (html->xexp (preprocess-html-wiki (jp "/parse/text" data))))
  (define tree (update-tree-wiki page wikiname))
  (remove-duplicates
   (for/list ([element (in-producer
                        (query-selector
                         (λ (t a c)
                           (and (eq? t 'img)
                                (get-attribute 'src a)))
                         tree)
                        #f)])
     (image-url->values (get-attribute 'src (bits->attributes element))))))

;; 5. Download image sources and style images according to database
(define (save-each-image wikiname source callback)
  ;; gather list of basenames to download (that aren't yet complete)
  (define rows (query-rows slc "select url, hash from image where wikiname = ? and source <= ? and progress < 1"
                                wikiname source))
  ;; counter of complete/incomplete basenames
  (define already-done-count
    (query-value slc "select count(*) from image where wikiname = ? and source <= ? and progress = 1"
                 wikiname source))
  (define not-done-count
    (query-value slc "select count(*) from image where wikiname = ? and source <= ? and progress < 1"
                 wikiname source))
  ;; set initial progress
  (callback already-done-count (+ already-done-count not-done-count) "")
  ;; loop through urls and download
  (for ([row rows]
        [i (in-naturals 1)])
    ;; row fragments
    (define url (vector-ref row 0))
    (define hash (vector-ref row 1))
    ;; check
    (printf "~a -> ~a~n" url hash)
    (define r (get url))
    (define declared-type (response-headers-ref r 'content-type))
    (define final-type (if (equal? declared-type #"application/octet-stream")
                           (let ([sniff-entity (message-entity (mime-analyze (response-body r)))])
                             (string->bytes/latin-1 (format "~a/~a" (entity-type sniff-entity) (entity-subtype sniff-entity))))
                           declared-type))
    (define ext (bytes->string/latin-1 (mime-type->ext final-type)))
    ;; save
    (define save-dir (hash->save-dir wikiname hash))
    (make-directory* save-dir)
    (define save-path (build-path save-dir (string-append hash "." ext)))
    (define body (response-body r))
    (display-to-file body save-path #:exists 'replace)
    (query-exec slc "update image set progress = 1, ext = ? where wikiname = ? and hash = ?"
                ext wikiname hash)
    (callback (+ already-done-count i) (+ already-done-count not-done-count) (string-append hash "." ext)))
  ;; TODO: saved all images, register that fact in the database
  )


(module+ test
  (check-equal? (html->xexp "<img src=\"https://example.com/images?src=Blah.jpg&amp;width=150\">")
                '(*TOP* (img (@ (src "https://example.com/images?src=Blah.jpg&width=150")))))
  #;(download-list-of-pages "minecraft" values)
  #;(save-each-page "minecraft" values)
  #;(check-json-for-images "chiki" (build-path archive-root "chiki" "Fiona.json"))
  #;(do-step-3 "gallowmere")
  #;(save-each-image "gallowmere" (hash-ref sources 'style) (λ (a b c) (printf "~a/~a ~a~n" a b c)))

  #;(for ([wikiname (query-list slc "select wikiname from wiki")])
      (println wikiname)
      (insert-wiki-entry wikiname))

  #;(for ([wikiname (query-list slc "select wikiname from wiki")])
      (println wikiname)
      (do-step-3 wikiname)
      (save-each-image wikiname (hash-ref sources 'style) (λ (a b c) (printf "~a/~a ~a~n" a b c)))))

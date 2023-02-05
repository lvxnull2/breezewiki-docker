#lang racket/base
(require racket/generator
         racket/list
         racket/string
         json
         net/http-easy
         "../lib/html-parsing/main.rkt"
         "../src/xexpr-utils.rkt"
         "../src/url-utils.rkt")

(define output-file "wiki-names.json")
(define limit "5000")

(define (get-page offset)
  (define res (get (format "https://community.fandom.com/wiki/Special:NewWikis?~a"
                           (params->query `(("offset" . ,offset)
                                            ("limit" . ,limit))))))
  (html->xexp (bytes->string/utf-8 (response-body res))))

(define (convert-list-items gen)
  (for/list ([item (in-producer gen #f)])
    ; '(li "\n" "\t" (a (@ (href "http://terra-hexalis.fandom.com/")) "Terra Hexalis Wiki") "\n" "\t\t\ten\t")
    (hasheq 'title (third (fourth item))
            'link (second (second (second (fourth item))))
            'lang (string-trim (sixth item)))))

(define (get-items-recursive [offset ""] [items null])
  (define page (get-page offset))
  (define page-content ((query-selector (attribute-selector 'class "mw-spcontent") page)))
  (define next ((query-selector (attribute-selector 'class "mw-nextlink") page-content)))
  (define next-offset
    (if next
        (second (regexp-match #rx"offset=([0-9]*)" (get-attribute 'href (bits->attributes next))))
        #f))
  (define list-item-generator (query-selector (λ (e a c) (eq? e 'li)) page-content))
  (define these-items (convert-list-items list-item-generator))
  (define all-items (append items these-items))
  (printf "page offset \"~a\" has ~a items (~a so far)~n" offset (length these-items) (length all-items))
  (if next
      (get-items-recursive next-offset all-items)
      all-items))

(call-with-output-file output-file #:exists 'truncate/replace
  (λ (out)
    (write-json (get-items-recursive) out)))

#lang typed/racket/base
(require racket/string
         "pure-utils.rkt")
(require/typed "config.rkt" [config-true? (Symbol -> Boolean)])

(provide
 ; regex to match wiki names
 wikiname-regex
 ; make a query string from an association list of strings
 params->query
 ; make a proxied version of a fandom url
 u-proxy-url
 ; check whether a url is on a domain controlled by fandom
 is-fandom-url?
  ; prints "out: <url>"
 log-outgoing)

(module+ test
  (require "typed-rackunit.rkt"))

(define wikiname-regex "[a-zA-Z0-9-]{3,50}")

;; https://url.spec.whatwg.org/#urlencoded-serializing

(define urlencoded-set '(#\! #\' #\( #\) #\~ ; urlencoded set
                         #\$ #\% #\& #\+ #\, ; component set
                         #\/ #\: #\; #\= #\@ #\[ #\\ #\] #\^ #\| ; userinfo set
                         #\? #\` #\{ #\} ; path set
                         #\  #\" #\# #\< #\> ; query set
                         ; c0 controls included elsewhere
                         ; higher ranges included elsewhere
                         ))

(: percent-encode (String (Listof Char) Boolean -> Bytes))
(define (percent-encode value set space-as-plus)
  (define b (string->bytes/utf-8 value))
  (apply bytes-append
         (for/list ([char b]) : (Listof Bytes)
                   (cond
                     [(and space-as-plus (eq? char 32))
                      #"+"]
                     [(or (member (integer->char char) set)
                          (char . > . #x7E)
                          (char . <= . #x1F))
                      (bytes-append #"%" (string->bytes/latin-1
                                          (string-upcase (number->string char 16))))]
                     [#t
                      (bytes char)]))))

(: params->query ((Listof (Pair String String)) -> String))
(define (params->query params)
  (string-join
   (map (λ ([p : (Pair String String)])
          (format "~a=~a"
                  (percent-encode (car p) urlencoded-set #t)
                  (percent-encode (cdr p) urlencoded-set #t)))
        params)
   "&"))
(module+ test
  (check-equal? (params->query '(("hello" . "world")))
                "hello=world")
  (check-equal? (params->query '(("a" . "hello world''") ("utf8" . "✓")))
                "a=hello+world%27%27&utf8=%E2%9C%93"))

(: is-fandom-url? (String -> Boolean))
(define (is-fandom-url? url)
  (regexp-match? (pregexp (format "^https://static\\.wikia\\.nocookie\\.net/|^https://~a\\.fandom\\.com/" wikiname-regex)) url))
(module+ test
  (check-true (is-fandom-url? "https://static.wikia.nocookie.net/wikiname/images/2/2f/SomeImage.jpg/revision/latest?cb=20110210094136"))
  (check-true (is-fandom-url? "https://test.fandom.com/wiki/Some_Page"))
  (check-false (is-fandom-url? "https://cadence.moe")))

(: u-proxy-url (String -> String))
(define (u-proxy-url url)
  (u
   is-fandom-url?
   (λ ([v : String]) (string-append "/proxy?" (params->query `(("dest" . ,url)))))
   url))

(: log-outgoing (String -> Void))
(define (log-outgoing url-string)
  (when (config-true? 'log_outgoing)
    (printf "out: ~a~n" url-string)))

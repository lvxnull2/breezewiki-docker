#lang racket/base
(require web-server/http/request-structs
         net/url-structs
         (only-in racket/promise delay))
(provide
 test-req)

(define test-req (request #"GET" (url "https" #f "breezewiki.com" #f #t (list (path/param "" '())) '() #f) '() (delay '()) #f "127.0.0.1" 0 "127.0.0.1"))

#lang racket
(require net/url "middleware.rkt" "shared.rkt")
(provide client)
  
;; (or/c 'get 'post) string? #:content-type symbol? (dict-of symbol? any/c)
(define (client method url #:content-type type #:request-map [request-map null])
  (call-middleware (req method (string->url url) type request-map)))

;; req? -> resp?
(define (call-middleware req)
  ((for/fold ([call http-invoke]) ([m middleware])
     (m call))
   req))

;; req? -> resp?
(define (http-invoke req)
  ...)

  
    







#lang racket
(require net/url "middleware.rkt" "shared.rkt" "parse.rkt")
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
  (parse-impure-port
   ((case (req-method req)
      [(get)    get-impure-port]
      [(post)   post-impure-port]
      [(delete) delete-impure-port]
      [(put)    put-impure-port]
      [(head)   head-impure-port])
    (req-uri req)
    (build-headers req))))

(define (build-headers req)
  (cons
   (~a "content-type:" (req-content-type req))
   (for/list ([(f v) (in-dict (req-request-map req))])
     (~a f ":" v))))
   

  
    







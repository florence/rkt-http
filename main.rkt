#lang racket
(require net/url)
(provide 
 client
 (contract-out
  [middleware/c contract?]))

(define middleware/c (-> middleware/c (-> req? resp?)))
;; (or/c 'get 'post) uri? symbol? (dict-of symbol? string?)
(struct req (method uri content-type request-map) #:transparent)
;; number? any/c (dict-of symbol? any/c)
(struct resp (content-type status body header/raw headers) #:transparent)
  
;; (or/c 'get 'post) string? #:content-type symbol? (dict-of symbol? any/c)
(define (client method url #:content-type type #:request-map [request-map null])
  (call-middleware (req method (string->url url) type request-map)))

(define middleware null)

;; req? -> resp?
(define (call-middleware req)
  ((for/fold ([call http-invoke]) ([m middleware])
     (m call))
   req))

;; req? -> resp?
(define (http-invoke req)
  ...)

  
    







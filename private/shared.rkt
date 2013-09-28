#lang racket
(provide 
 request-map-ref
 request-map-set
 header-map-ref
 method/c
 (struct-out req)
 (struct-out resp))

(define method/c (or/c 'get 'post 'put 'delete 'head #f))

;; method/c uri? (or/c symbol? string?) (dict-of symbol? string?)
(struct req (method uri request-map) #:transparent)

;; string? number? string? any/c (dict-of symbol? any/c)
(struct resp (content-type code status body headers) #:transparent)

(define (request-map-ref req key [fail (lambda () #f)])
  (dict-ref (req-request-map req) key fail))
(define (request-map-set a-req key val)
  (struct-copy req a-req
               [request-map (dict-set (req-request-map a-req) key val)]))
(define (header-map-ref a-resp key [fail (lambda () #f)])
  (dict-ref (resp-headers a-resp) key fail))
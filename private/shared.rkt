#lang typed/racket
(provide 
 request-map-ref
 request-map-set
 header-map-ref
 Method
 (struct-out req)
 (struct-out resp))
(require "typed-conversions.rkt")

(define-type Method (Option (U 'get 'post 'put 'delete 'head)))
;(define method/c (or/c 'get 'post 'put 'delete 'head #f))

;; method/c uri? (or/c symbol? string?) (dict-of symbol? string?)
(struct: req ([method : Method]
              [uri : (U url String)]
              [request-map : (HashTable Symbol Any)])
  #:transparent)

;; string? number? string? any/c (dict-of symbol? any/c)
(struct: resp ([content-type : String] 
               [code : Positive-Integer]
               [status : String]
               [body : Any]
               [headers : (HashTable Symbol Any)]) 
  #:transparent)

(: request-map-ref : (case-> 
                      [req Symbol -> Any]
                      [req Symbol (-> Any) -> Any]))
(define (request-map-ref req key [fail (lambda () #f)])
  (hash-ref (req-request-map req) key fail))

(: request-map-set : (req Symbol Any -> req))
(define (request-map-set a-req key val)
  (struct-copy req a-req
               [request-map (hash-set (req-request-map a-req) key val)]))

(: header-map-ref : (case-> [resp Symbol -> Any]
                            [resp Symbol (-> Any) -> Any]))
(define (header-map-ref a-resp key [fail (lambda () #f)])
  (hash-ref (resp-headers a-resp) key fail))
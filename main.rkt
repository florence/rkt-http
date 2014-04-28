#lang typed/racket
(provide 
 ;; top level
 request request/no-process make-processor
 ;; types
 Method Processor 
 ;; structures
 (struct-out req)
 (struct-out resp))

(require typed/net/url)
(require "private/typed-conversions.rkt" 
         "processors.rkt"
         "private/shared.rkt"
         "private/parse.rkt"
         "private/processors.rkt")
(module+ test (require rackunit))

(define EMPTY-REQ-MAP ((inst hash Symbol Any)))

(: request : (Method String [#:request-map (HashTable Symbol Any)] [#:processors (Listof Processor)] -> resp))
(define (request method url #:request-map [request-map EMPTY-REQ-MAP] #:processors [processors default-processors])
  (call-middleware (req method (string->url url) request-map) processors))

(: request/no-process : (Method String -> resp)) 
(define (request/no-process method url)
  (request method url #:request-map EMPTY-REQ-MAP #:processors null))

(: call-middleware : (req (Listof Processor) -> resp))
(define (call-middleware req processors)
  (define: chain : Request-Response
    (flatten-processor-list http-invoke (reverse processors)))
   (chain req))

(: http-invoke : (req -> resp))
(define (http-invoke req)
  (: uri url)
  (define uri (let ([u (req-uri req)]) (if (string? u) (string->url u) u)))
  (define headers (build-headers req))
  (define post
    (let ([v (request-map-ref req 'post)])
      (if (not v) #"" (string->bytes/locale (~a v)))))
  (parse-impure-port
    (case (req-method req)
      [(get)    (get-impure-port uri headers)]
      [(post)   (post-impure-port uri post headers)]
      [(delete) (delete-impure-port uri headers)]
      [(put)    (put-impure-port uri post headers)]
      [(head)   (head-impure-port uri headers)]
      ;; TODO error class
      [(#f) (error 'http "method not set")])))

(: build-headers : (req -> (Listof String)))
(define (build-headers req)
  (for/list ([(f v) (in-hash (req-request-map req))])
    (~a f ":" v)))

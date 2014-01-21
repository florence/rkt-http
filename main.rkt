#lang typed/racket
(provide 
 ;; top level
 request request/no-process
 ;; types
 Method Processor 
 ;; structures
 (struct-out req)
 (struct-out resp)
 ;; processors
 no-op make-processor 
 body-convert
 (rename-out [processors default-processors]))

(require typed/net/url)
(require "private/typed-conversions.rkt" 
         "processors.rkt"
         "private/shared.rkt"
         "private/parse.rkt"
         "private/processors.rkt")
(module+ test (require rackunit))

(define EMPTY-REQ-MAP ((inst hash Symbol Any)))

(: request : (Method String [#:request-map (HashTable Symbol Any)] [#:processors (Listof Processor)] -> resp))
(define (request method url #:request-map [request-map EMPTY-REQ-MAP] #:processors [processors processors])
  (call-middleware (req method (string->url url) request-map) processors))

(: request/no-process : (Method String -> resp)) 
(define (request/no-process method url)
  (request method url #:request-map EMPTY-REQ-MAP #:processors null))

(: call-middleware : (req (Listof Processor) -> resp))
(define (call-middleware req processors)
  (define: chain : Request-Response
    (for/fold ([call http-invoke]) ([p processors])
     (define processor
       (if (not (processor-parameter-wrapper? p))
           p
           ((processor-parameter-wrapper-processor p))))
     (processor call)))
   (chain req))

(: http-invoke : (req -> resp))
(define (http-invoke req)
  (define: impure : (url (Listof String) -> Input-Port)
    (case (req-method req)
      [(get)    get-impure-port]
      [(post)   (error 'http "method not implemented")#;post-impure-port]
      [(delete) delete-impure-port]
      [(put)    (error 'http "method not implemented")#;put-impure-port]
      [(head)   head-impure-port]
      ;; TODO error class
      [(#f) (error 'http "method not set")]))
  (define uri (let ([u (req-uri req)]) (if (string? u) (string->url u) u)))
  (parse-impure-port
   (impure
    uri
    (build-headers req))))

(: build-headers : (req -> (Listof String)))
(define (build-headers req)
  (for/list ([(f v) (in-hash (req-request-map req))])
    (~a f ":" v)))

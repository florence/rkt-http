#lang racket
#|
this module provides the basic middleware for rkt-http
|#
(provide 
 (contract-out
  [middleware/c contract?]
  [make-middleware (->* ()
                        (#:req (-> req? req?) #:resp (-> resp? resp?))
                        middleware/c)]))
                    
(require "private/shared.rkt" net/url)
(module+ test (require rackunit))


(define middleware/c (recursive-contract (-> middleware/c (-> req? resp?))))


(define RETRY-LIMIT 10)
    
    
(define-syntax (create-middleware stx)
  (syntax-case stx ()
    [(_ [name thunk] ...)
     #'(begin
         (provide middleware name ...)
         (define name (make-parameter thunk)) ...
         (define middleware (list name ...)))]))

(define (make-middleware #:req [req values] #:resp [resp values])
  (lambda (client)
    (lambda (r)
      (resp (client (req r))))))

(create-middleware
 [lowercase-headers
  (make-middleware
   #:req
   (lambda (a-req)
     (define cur-headers (request-map-ref a-req 'header))
     (cond
       [cur-headers
        (define new-headers
          (for/hash ([(k v) (in-dict cur-headers)])
            (values (string->symbol (string-downcase (~a k))) v)))
        (request-map-set a-req 'header new-headers)]
       [else a-req])))]
 [redirect
  (lambda (client)
    (lambda (a-req)
      (let loop ([count 0] [a-req a-req])
        (define a-resp (client req))
        (if (or (not (eq? 302 (resp-code resp))) (eq? count RETRY-LIMIT))
            resp
            (loop (add1 count)
                  (struct-copy req a-req
                               [uri (string->url (request-map-ref req 'location))]))))))]
 [content-type
  (make-middleware
   #:req 
   (lambda (a-req)
     (define ctype (request-map-ref a-req 'content-type))
     (cond
       [ctype
        (define parsed-ctype 
          (if (string? ctype)
              ctype
              (string-append "application/" (~a ctype))))
        (request-map-set a-req 'content-type parsed-ctype)]
       [else a-req])))])
  
  

(module+ test
  (define (check-middleware middleware input output)
    (check-equal? (((middleware) values) input)
                  output))
  (check-middleware content-type 
                    (req 'get #f #hash((content-type . json)))
                    (req 'get #f #hash((content-type . "application/json"))))
  (check-middleware content-type 
                    (req 'get #f null)
                    (req 'get #f null)))
  
  
      
  
  
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
                    
(require "shared.rkt")
(module+ test (require rackunit))


(define middleware/c (recursive-contract (-> middleware/c (-> req? resp?))))

(define-syntax (create-middleware stx)
  (syntax-case stx ()
    [(_ [(name callback req) body ...] ...)
     #'(begin
         (provide middleware name ...)
         (define name 
           (make-parameter
            (lambda (callback)
              (lambda (req)
                body ...)))) ...
         (define middleware (list name ...)))]))
        



(create-middleware

 [(content-type client a-req)
  (define ctype (req-content-type a-req)) 
  (define parsed-ctype 
    (if (string? ctype)
        ctype
        (string-append "application/" (~a ctype))))
  (define nrequest
    (struct-copy req a-req
                 [content-type parsed-ctype]))
  (client nrequest)])





(define (make-middleware #:req [req values] #:resp [resp values])
  (lambda (client)
    (lambda (r)
      (resp (client (req r))))))
  
  

(module+ test
  (define (check-middleware middleware input output)
    (check-equal? (((middleware) values) input)
                  output))
  (check-middleware content-type 
                    (req 'get #f 'json null)
                    (req 'get #f "application/json" null)))
  
  
      
  
  
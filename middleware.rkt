#lang racket
#|
this module provides the basic middleware for rkt-http
|#
(provide middleware/c)
(require "shared.rkt")


(define middleware/c (recursive-contract (-> middleware/c (-> req? resp?))))

(define-syntax (create-middleware stx)
  (syntax-case stx ()
    [(_ [(name callback req) body ...] ...)
     #'(begin
         (provide middleware name ...)
         (define middleware (list name ...))
         (define ((name callback) req)
           body ...) ...)]))
        
        

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
  
  
  
      
  
  
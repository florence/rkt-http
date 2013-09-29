#lang racket
#|
this module provides the basic middleware for rkt-http
|#
(provide 
 (contract-out
  [processor/c contract?]
  [make-processor (->* ()
                        (#:req (-> req? req?) #:resp (-> resp? resp?))
                        processor/c)]))

(require "private/shared.rkt" 
         "parsers.rkt"
         net/url)
(module+ test (require rackunit))


(define request-response/c (-> req? resp?))
(define processor/c (-> request-response/c request-response/c))


(define RETRY-LIMIT 10)


(define-syntax (create-processor stx)
  (syntax-case stx ()
    [(_ [name thunk] ...)
     #'(begin
         (provide (contract-out
                   [processors (listof (parameter/c processor/c))]
                   [name (parameter/c processor/c)] ...))
         (define name (make-parameter thunk)) ...
         (define processors (list name ...)))]))

(define (make-processor #:req [req values] #:resp [resp values])
  (lambda (client)
    (lambda (r)
      (resp (client (req r))))))


;; request only middleware to lowercase all header field names
(define in:lowercase-headers
  (make-processor
   #:req
   (lambda (a-req)
     (define cur-headers (request-map-ref a-req 'header))
     (cond
       [cur-headers
        (define new-headers
          (for/hash ([(k v) (in-dict cur-headers)])
            (values (string->symbol (string-downcase (~a k))) v)))
        (request-map-set a-req 'header new-headers)]
       [else a-req]))))
;; middleware to control redirecting
(define in:redirect
  (lambda (client)
    (lambda (a-req)
      (let loop ([count 0] [a-req a-req])
        (define a-resp (client a-req))
        (if (or (not (= 301 (resp-code a-resp))) (= count RETRY-LIMIT))
            a-resp
            (loop (add1 count)
                  (struct-copy req a-req
                               [uri (string->url (~a (header-map-ref a-resp 'location)))])))))))
;; request only middleware to handle setting the content-type header
(define in:content-type
  (make-processor
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
       [else a-req]))))
;; middleware to convert xexprs and that ilk to strings given the content type
(define in:body-convert
  (make-processor
   #:req (compose json-request-body-converter xml-request-body-converter)
   #:resp (compose json-resp-body-converter xml-resp-body-converter)))


(create-processor
 [lowercase-headers in:lowercase-headers]
 [redirect in:redirect]
 ;; header processor
 [content-type in:content-type]
 ;; final processing
 [body-convert in:body-convert]
 [retry values])


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





#lang racket
#|
this module provides the basic middleware for rkt-http
|#
(provide 
 (contract-out
  [no-op processor/c]
  [make-processor (->* ()
                        (#:req (-> req? req?) #:resp (-> resp? resp?))
                        processor/c)]))

(require "private/shared.rkt" 
         "private/processors.rkt"
         "parsers.rkt"
         net/url)
(module+ test (require rackunit))


(define RETRY-LIMIT 10)


(define-syntax (create-processor stx)
  (syntax-case stx ()
    [(_ [name thunk] ...)
     (with-syntax ([(n ...) (generate-temporaries #'(name ...))])
     #'(begin
         (provide (contract-out
                   [processors (listof processor/c)]
                   [name (parameter/c processor/c)] ...))
         (define n (wrap-processor thunk)) ...
         (define name (processor-parameter-wrapper-processor n)) ...
         (define processors (list n ...))))]))

(define (make-processor #:req [req values] #:resp [resp values])
  (lambda (client)
    (lambda (r)
      (resp (client (req r))))))


;; request only processor to lowercase all header field names
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
;; processor to control redirecting
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
;; request only processor to handle setting the content-type header
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
;; processor to convert xexprs and that ilk to strings given the content type
(define in:body-convert
  (make-processor
   #:req (compose json-request-body-converter xml-request-body-converter)
   #:resp (compose json-resp-body-converter xml-resp-body-converter)))
;; a no-op processor
(define no-op values)

(create-processor
 [lowercase-headers in:lowercase-headers]
 [redirect in:redirect]
 ;; header processor
 [content-type in:content-type]
 ;; final processing
 [body-convert in:body-convert]
 [retry values])


(module+ test
  (define (check-req-processor processor input output)
    (check-equal? (((processor) values) input)
                  output))
  (check-req-processor content-type 
                    (req 'get #f #hash((content-type . json)))
                    (req 'get #f #hash((content-type . "application/json"))))
  (check-req-processor content-type 
                    (req 'get #f null)
                    (req 'get #f null))
  (check-req-processor lowercase-headers
                    (req 'get #f #hash((header . #hash((TestHEadEr . "value1")
                                                        ("E3xDg" . "value2")))))
                    (req 'get #f #hash((header . #hash((testheader . "value1")
                                                        (e3xdg . "value2"))))))
  ;; testing the default redirect client
  (let ()
    (define req1 (req 'get #f null))
    (define response1 (resp "text/tsxt" 301 "anything" "" null))
    (define response2 (resp "text/tsxt" 500 "anything" "" null))
    (define (client v)
      (if (equal? v req1)
          response1
          response2))
    (check-equal?
     (((redirect) client) req1)
     response2)))





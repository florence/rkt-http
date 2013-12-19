#lang typed/racket
#|
this module provides the basic middleware for rkt-http
|#
(provide no-op make-processor)

(require "private/shared.rkt" 
         "private/processors.rkt"
         "parsers.rkt"
         "private/typed-conversions.rkt")



(define RETRY-LIMIT 10)


(define-syntax (create-processor stx)
  (syntax-case stx ()
    [(_ [name thunk] ...)
     (with-syntax ([(n ...) (generate-temporaries #'(name ...))])
     #'(begin
         (provide processors name ...)
         (define n (wrap-processor thunk)) ...
         (define name (processor-parameter-wrapper-processor n)) ...
         (define processors (list n ...))))]))

(: make-processor : ([#:req (req -> req)] [#:resp (resp -> resp)] -> FProcessor))
(define (make-processor #:req [req* values] #:resp [resp* values])
  (lambda: ([client : (req -> resp)])
    (lambda: ([r : req])
      (resp* (client (req* r))))))


;; request only processor to lowercase all header field names
(define in:lowercase-headers
  (make-processor
   #:req
   (lambda: ([a-req : req])
     (define cur-headers (request-map-ref a-req 'header))
     (if (not (hash? cur-headers)) ;; no headers given
         a-req
         (cond
           [cur-headers
            (define: new-headers : (HashTable Any Any)
              (for/hash ([(k v) (in-hash cur-headers)])
                (values (string->symbol (string-downcase (~a k))) v)))
            (request-map-set a-req 'header new-headers)]
           [else a-req])))))
;; processor to control redirecting
(define: in:redirect : FProcessor
   (lambda: ([client : (req -> resp)])
     (lambda: ([a-req : req])
       (let: loop : resp ([count : Natural 0] [a-req : req a-req])
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
   (lambda: ([a-req : req])
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

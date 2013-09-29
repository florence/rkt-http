#lang racket
#|
this module provides bindings to convert request and response bodies to and from text based on the the content-type
|#
(provide
 (contract-out
  [json-resp-body-converter    (-> resp? resp?)]
  [xml-resp-body-converter     (-> resp? resp?)]
  [json-request-body-converter (-> req? req?)]
  [xml-request-body-converter  (-> req? req?)]))
(require "private/shared.rkt"
         (prefix-in js: json)
         (prefix-in x: xml))
(module+ test (require rackunit net/url))

;; (R -> boolean?) (V -> P) (R -> (or/c V #f)) (R P -> R) -> (R -> R)
;; makes a function that takes in an R, checks if it matches recognize?, and sets the value via parse
(define ((make-body-converter recognize? parse get set) r)
  (define body (get r))
  (cond
    [(and body (recognize? r))
     (set r (parse body))]
    [else r]))
;; (R -> any/c) (R -> any/c) -> (R -> boolean)
;; does this have a jsexpr body and an application/json content-type
(define ((json-body? body-getter header-getter) r)
  (and (equal? (header-getter r) "application/json")
       (js:jsexpr? (body-getter r))))

(define json-parse js:jsexpr->string)
(define json-unparse js:string->jsexpr)
;; (R -> any/c) (R -> any/c) -> (R -> boolean)
;; does this have a xexpr body and an application/TODO content-type
(define ((xml-body? body-getter header-getter) r)
  (and (equal? (header-getter r) "text/xml")
       (x:xexpr? (body-getter r))))

(define xml-parse x:xexpr->string)
(define xml-unparse x:string->xexpr)

;; request bodyparsers

;; (req? -> boolean?) (V -> P) -> (req? -> req?)
;; like make-body-converter where R = req?
(define (make-request-body-converter recognize? parse)
  (make-body-converter recognize? parse getter-req setter-req))

(define (getter-req a-req) (request-map-ref a-req 'body))
(define (get-header-req r) (request-map-ref r 'content-type))
(define (setter-req a-req val) (request-map-set a-req 'body val))

(define json-request-body? 
  (json-body? getter-req get-header-req))
(define xml-request-body? 
  (xml-body? getter-req get-header-req))

(define json-request-body-converter
  (make-request-body-converter json-request-body? json-parse))
(define xml-request-body-converter
  (make-request-body-converter xml-request-body? xml-parse))

;; response body parsers

;; (resp? -> boolean?) (V -> P) -> (resp? -> resp?)
;; like make-body-converter where R = resp?
(define (make-response-body-converter recognize? parse)
  (make-body-converter recognize? parse getter-resp setter-resp))

(define getter-resp resp-body)
(define get-header-resp resp-content-type)
(define (setter-resp a-resp val) 
  (struct-copy resp a-resp [body val]))

(define json-resp-body? 
  (json-body? getter-resp get-header-resp))
(define xml-resp-body? 
  (xml-body? getter-resp get-header-resp))

(define json-resp-body-converter
  (make-response-body-converter json-resp-body? json-unparse))
(define xml-resp-body-converter
  (make-response-body-converter xml-resp-body? xml-unparse))

(module+ test
  ;; json req
  (let ()
    (define r (req 'get (string->url "http://test.com")
                   #hash()))
    (check-equal?
     (json-request-body-converter r)
     r))
  (let () 
    (define method 'get)
    (define uri (string->url "http://test.com"))
    (define r (req method uri
                   `#hash((content-type . "application/json")
                          (body . ,(hasheq)))))
    (check-equal?
     (json-request-body-converter r)
     (req method uri 
          #hash((body . "{}")
                (content-type . "application/json")))))
  ;; json resp
  (let ()
    (define r (resp "text/html" 200 "OK" "" #hash((content-type . "text/html; charset=UTF-8"))))
    (check-equal?
     (json-resp-body-converter r)
     r))
  (let () 
    (define r (resp "application/json" 200 "OK" "{}" #hash((content-type . "application/json; charset=UTF-8"))))
    (check-equal?
     (json-resp-body-converter r)
     (resp "application/json" 200  "OK"
           (hasheq)
           #hash((content-type . "application/json; charset=UTF-8")))))
  ;; xml req
  (let ()
    (define r (req 'get (string->url "http://test.com")
                   #hash()))
    (check-equal?
     (xml-request-body-converter r)
     r))
  (let () 
    (define method 'get)
    (define uri (string->url "http://test.com"))
    (define r (req method uri
                   `#hash((content-type . "text/xml")
                          (body . (test ())))))
    (check-equal?
     (xml-request-body-converter r)
     (req method uri 
          #hash((body . "<test></test>")
                (content-type . "text/xml")))))
  ;; xml resp
  (let ()
    (define r (resp "text/html" 200 "OK" "" #hash((content-type . "text/html; charset=UTF-8"))))
    (check-equal?
     (xml-resp-body-converter r)
     r))
  (let () 
    (define r (resp "text/xml" 200 "OK" "<test/>" #hash((content-type . "text/xml; charset=UTF-8"))))
    (check-equal?
     (xml-resp-body-converter r)
     (resp "text/xml" 200  "OK"
           '(test ())
           #hash((content-type . "text/xml; charset=UTF-8"))))))

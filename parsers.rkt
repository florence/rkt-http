#lang typed/racket
#|
this module provides bindings to convert request and response bodies to and from text based on the the content-type
|#
(provide
  json-resp-body-converter
  xml-resp-body-converter
  json-request-body-converter
  xml-request-body-converter)
(require "private/shared.rkt")
(require/typed json
              [#:opaque JSexpr jsexpr?]
              [(jsexpr->string js:jsexpr->string) (JSexpr -> String)]
              [(string->jsexpr js:string->jsexpr) (String -> JSexpr)])
(require/typed xml
               [#:opaque Xexpr xexpr?]
               [(xexpr->string x:xexpr->string) (Xexpr -> String)]
               [(string->xexpr x:string->xexpr) (String -> Xexpr)])
(module+ test 
  (require typed/rackunit "private/typed-conversions.rkt"))

(: make-body-converter : (All (R V P) ((R -> Any) (V -> P) (R -> V) (R P -> R) -> (R -> R))))
;; makes a function that takes in an R, checks if it matches recognize?, and sets the value via parse
(define ((make-body-converter recognize? parse get set) r)
  (define body (get r))
  (cond
    [(and body (recognize? r))
     (set r (parse body))]
    [else r]))

(: json-body? : (All (R) ((R -> Any) (R -> Any) -> (R -> (Option Any)))))
;; does this have a jsexpr body and an application/json content-type
(define ((json-body? body-getter header-getter) r)
  (and (equal? (header-getter r) "application/json")
       (body-getter r)))

(: json-parse : (Any -> Any))
(define (json-parse v) 
  (if (jsexpr? v) 
      (js:jsexpr->string v)
      v))
(: json-unparse : (Any -> Any))
(define (json-unparse v)
  (if (string? v)
      (js:string->jsexpr v)
      v))

(: xml-body? : (All (R) ((R -> Any) (R -> Any) -> (R -> (Option Any)))))
;; does this have a xexpr body and an application/TODO content-type
(define ((xml-body? body-getter header-getter) r)
  (and (equal? (header-getter r) "text/xml")
       (body-getter r)))

(: xml-parse : (Any -> Any))
(define (xml-parse v)
  (if (xexpr? v)
      (x:xexpr->string v)
      v))
(: xml-unparse : (Any -> Any))
(define (xml-unparse v) 
  (if (string? v)
      (x:string->xexpr v)
      v))

;; request bodyparsers

(: make-request-body-converter : (All (P) ((req -> Any) (Any -> P) -> (req -> req))))
;; like make-body-converter where R = req?
(define (make-request-body-converter recognize? parse)
  ((inst make-body-converter req Any P) recognize? parse getter-req setter-req))

(: getter-req : (req -> Any))
(define (getter-req a-req) (request-map-ref a-req 'body))
(: get-header-req : (req -> Any))
(define (get-header-req r) (request-map-ref r 'content-type))
(: setter-req : (req Any -> req))
(define (setter-req a-req val) (request-map-set a-req 'body val))

(define json-request-body? (json-body? getter-req get-header-req))
(define xml-request-body? (xml-body? getter-req get-header-req))


(define: json-request-body-converter : (req -> req)
  (make-request-body-converter json-request-body? json-parse))
(define: xml-request-body-converter : (req -> req)
  (make-request-body-converter xml-request-body? xml-parse))

;; response body parsers

(: make-response-body-converter : (All (P) ((resp -> Any) (Any -> P) -> (resp -> resp))))
;; like make-body-converter where R = resp?
(define (make-response-body-converter recognize? parse)
  ((inst make-body-converter resp Any P) recognize? parse getter-resp setter-resp))

(define getter-resp resp-body)
(define get-header-resp resp-content-type)
(: setter-resp : (resp Any -> resp))
(define (setter-resp a-resp val) 
  (struct-copy resp a-resp [body val]))

(define json-resp-body? (json-body? getter-resp get-header-resp))
(define xml-resp-body?  (xml-body? getter-resp get-header-resp))

(define: json-resp-body-converter : (resp -> resp)
  (make-response-body-converter json-resp-body? json-unparse))
(define: xml-resp-body-converter : (resp -> resp)
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
                          (body . ,(cast (hash) Any)))))
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
           (cast (hash) Any)
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

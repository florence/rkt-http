#lang typed/racket
(require "../parsers.rkt" typed/rackunit "../private/typed-conversions.rkt" "../private/shared.rkt" typed/net/url)
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
         (make-immutable-hasheq)
         #hash((content-type . "application/json; charset=UTF-8")))))
(let () 
  (define r (resp "application/json; charset=utf-8" 400 "OK" "{}" #hash((content-type . "application/json; charset=UTF-8"))))
  (check-equal?
   (json-resp-body-converter r)
   (resp "application/json; charset=utf-8" 400  "OK"
         (make-immutable-hasheq)
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
;; xml can't be empty?
#;(let ()
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
         #hash((content-type . "text/xml; charset=UTF-8")))))

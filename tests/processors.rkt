#lang typed/racket
(require "../processors.rkt" "../private/processors.rkt" typed/rackunit "../private/typed-conversions.rkt" "../private/shared.rkt")
(: check-req-processor : ((Parameterof Processor) req req -> Any))
(define (check-req-processor processor input output)
  (define (check req)
    (check-equal? req output)
    (resp "text/tsxt" 301 "anything" "" (hash)))
  (void (((processor) check) input)))
(check-req-processor content-type 
                     (req 'get "" #hash((content-type . json)))
                     (req 'get "" #hash((content-type . "application/json"))))
(check-req-processor content-type 
                     (req 'get "" (hash))
                     (req 'get "" (hash)))
(check-req-processor lowercase-headers
                     (req 'get "" #hash((header . #hash((TestHEadEr . "value1")
                                                        ("E3xDg" . "value2")))))
                     (req 'get "" #hash((header . #hash((testheader . "value1")
                                                        (e3xdg . "value2"))))))
;; testing the default redirect client
(let ()
  (define req1 (req 'get "" (hash)))
  (define response1 (resp "text/tsxt" 301 "anything" "" (hash)))
  (define response2 (resp "text/tsxt" 500 "anything" "" (hash)))
  (define (client v)
    (if (equal? v req1)
        response1
        response2))
  (check-equal?
   (((redirect) client) req1)
   response2))

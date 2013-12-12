#lang typed/racket
(provide parse-impure-port)
(require "shared.rkt" "typed-conversions.rkt")
(require/typed racket/base [bytes->string/utf-8 (Bytes -> String)])
;(require web-server/http/request web-server/http net/url)
;(require/typed web-server/http/request)
;(require/typed web-server/http)
(module+ test (require typed/rackunit))

;; iport -> resp?
(: parse-impure-port : (Input-Port -> resp))
(define (parse-impure-port port)
  (define status-line (read-line port))
  (cond [(eof-object? status-line)
         (error 'parse-impure-port "given closed port")]
        [else 
         (define-values (code status) (parse-status-line status-line))
         (define headers/raw (read-headers port))
         (define headers
           (for/hash: : (HashTable Symbol String) ([h : header headers/raw])
             (values (string->symbol (string-downcase (bytes->string/utf-8 (header-field h))))
                     (bytes->string/utf-8 (header-value h)))))
         (resp (hash-ref headers 'content-type)
               code
               status
               (port->string port)
               (cast headers (HashTable Symbol Any)))]))
  

(: parse-status-line : (String -> (Values Positive-Integer String)))
(define (parse-status-line line)
  (define c (string->number (substring line 9 12)))
  (if (not (exact-positive-integer? c))
      (error 'parse-status-line "didn't find status number")
      (values
       c
       (substring line 13 (sub1 (string-length line))))))
  
(module+ test
  (let ()
   (define-values (code status) (parse-status-line "HTTP/1.0 420 Enhance Your Calm\r"))
   (check-equal? code 420)
   (check-equal? status "Enhance Your Calm")))
  



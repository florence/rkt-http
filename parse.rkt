#lang racket
(provide parse-impure-port)
(require web-server/http/request web-server/http net/url "shared.rkt")
(module+ test (require rackunit))

;; iport -> resp?
(define (parse-impure-port port)
  (define status-line (read-line port))
  (define-values (code status) (parse-status-line status-line))
  (define headers/raw (read-headers port))
  (define headers (map (lambda (h) (cons (string->symbol (string-downcase (bytes->string/utf-8 (header-field h))))
                                         (header-value h)))
                       headers/raw))
  (resp (dict-ref headers 'content-type)
        code
        status
        (port->string port) headers))
  

;; string -> int string
(define (parse-status-line line)
  (values
   (string->number (substring line 9 12))
   (substring line 13 (sub1 (string-length line)))))

(module+ test
  (let ()
   (define-values (code status) (parse-status-line "HTTP/1.0 420 Enhance Your Calm\r"))
   (check-equal? code 420)
   (check-equal? status "Enhance Your Calm")))
  



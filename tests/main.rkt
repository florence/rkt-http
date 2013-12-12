#lang typed/racket
(require typed/rackunit 
         "../private/shared.rkt"
         "../processors.rkt"
         "../main.rkt")

(let ()
  (define resp (request 'get "http://www.google.com"))
  (check-equal? (resp-code resp) 200))
(let ()
  (parameterize ([retry values]) ;;testing parameterize the processors
    (define resp (request 'get "http://www.google.com"))
    (check-equal? (resp-code resp) 200))) 
(let ()
  (check-exn (lambda (x) (and x #t)) (thunk (request #f "http://www.google.com"))))
(let ()
  ;; this redirects, testing that
  (define resp (request 'get "http://google.com"))
  (check-equal? (resp-code resp) 200))

(let ()
  (define resp (request 'get 
                        "http://www.google.com"
                        #:processors (list (retry))))
  (check-equal? (resp-code resp) 200))
#lang typed/racket
(require typed/rackunit 
         "../processors.rkt"
         "../main.rkt")
(with-processors ([body-convert no-op])
  (let ()
    (define resp (request 'get "http://www.google.com"))
    (check-equal? (resp-code resp) 200))
  (let ()
    (with-processors ([retry no-op]) ;;testing with-processors the processors
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
    (check-equal? (resp-code resp) 200)))

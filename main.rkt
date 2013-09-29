#lang racket
(provide 
 (contract-out
  [request (->* (method/c string?) (#:request-map dict?) resp?)]
  [request/no-process (-> method/c string? resp?)]
  [request/processors
   (->* (method/c string?) (#:request-map dict? #:processors (listof middleware/c)) resp?)]
  [method/c contract?]
  [middleware/c contract?])
 (struct-out req)
 (struct-out resp))

(require net/url "middleware.rkt" "private/shared.rkt" "private/parse.rkt")
(module+ test (require rackunit))

(define (request method url #:request-map [request-map null])
  (request/processors method url #:request-map request-map))

(define (request/no-process method url)
  (request/processors method url #:request-map null #:processors null))

(define (request/processors method url #:request-map [request-map null] #:processors [processors middleware])
  (call-middleware (req method (string->url url) request-map) processors))

;; req? -> resp?
(define (call-middleware req middleware)
  ((for/fold ([call http-invoke]) ([m middleware])
     ((m) call))
   req))

;; req? -> resp?
(define (http-invoke req)
  (parse-impure-port
   ((case (req-method req)
      [(get)    get-impure-port]
      [(post)   post-impure-port]
      [(delete) delete-impure-port]
      [(put)    put-impure-port]
      [(head)   head-impure-port]
      ;; TODO error class
      [(#f) (error 'http "method not set")])
    (req-uri req)
    (build-headers req))))

(define (build-headers req)
  (for/list ([(f v) (in-dict (req-request-map req))])
    (~a f ":" v)))


(module+ test 
  (let ()
    (define resp (request 'get "http://www.google.com"))
    (check-equal? (resp-code resp) 200))
  
  (let ()
    (check-exn values (thunk (request #f "http://www.google.com"))))
  (let ()
    ;; this redirects, testing that
    (define resp (request 'get "http://google.com"))
    (check-equal? (resp-code resp) 200)))











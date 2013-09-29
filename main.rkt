#lang racket
(provide 
 (contract-out
  [request (->* (method/c string?) 
                (#:request-map dict? #:processors (listof processor/c))
                resp?)]
  [request/no-process (-> method/c string? resp?)]
  [method/c contract?]
  [processor/c contract?]
  [make-processor (->* ()
                       (#:req (-> req? req?) #:resp (-> resp? resp?))
                       processor/c)])
 (rename-out [processors default-processors])
 (struct-out req)
 (struct-out resp))

(require net/url "processors.rkt" "private/shared.rkt" "private/parse.rkt")
(module+ test (require rackunit))

(define (request method url #:request-map [request-map null] #:processors [processors processors])
  (call-middleware (req method (string->url url) request-map) processors))

(define (request/no-process method url)
  (request method url #:request-map null #:processors null))

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











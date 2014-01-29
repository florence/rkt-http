#lang racket
(require rkt-http rkt-http/processors json net/url)
(define BASE-URL "https://api.github.com/")



(define (entry r)
  (define user (request-map-ref r 'gists-for))
  (if (not user)
      r
      (let ()
       (define uri (req-uri r))
       (define new-url (struct-copy url uri
                                    [path (map (curryr path/param null) (list "users" user "gists"))]))
       (define r2 (struct-copy req r [uri new-url]))
       (displayln (url->string (req-uri r2)))
       r2)))
      

(define gist (make-processor entry values))

(request 'get BASE-URL
         #:request-map '#hash((gists-for . "florence"))
         #:processors (cons gist default-processors))


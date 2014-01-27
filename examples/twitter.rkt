#lang racket
(require rkt-http rkt-http/processors json)
(define BASE-URL "https://api.twitter.com/1.1/statuses/user_timeline.json")



(define (entry r)
  (define a (request-map-ref r 'twitter))
  (if (not a)
      r
      (let ()
        (define body 
          `#hash((user_name   . ,(hash-ref a 'username json-null))
                 (screen_name . ,(hash-ref a 'screenname json-null))
                 (count       . ,(hash-ref a 'count json-null))))
        (request-map-set a 'body body))))

(define twitter (make-processor #:req entry))

(request 'get BASE-URL
         #:request-map #hash((twitter . #hash((screenname . "RealmOfRacket")
                                              (count      . 5))))
         #:processors (cons twitter default-processors))

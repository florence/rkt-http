#lang racket
(provide 
 (struct-out req)
 (struct-out resp))


;; (or/c 'get 'post) uri? (or/c symbol? string?) (dict-of symbol? string?)
(struct req (method uri content-type request-map) #:transparent)
;; number? any/c (dict-of symbol? any/c)
(struct resp (content-type status body header/raw headers) #:transparent)
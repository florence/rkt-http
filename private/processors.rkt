#lang typed/racket
;; private bindings for processors
(provide Request-Response Processor wrap-processor)
(require "shared.rkt")


(define-type Request-Response (req -> resp))
(define-type Processor (Request-Response -> Request-Response))
(: wrap-processor : (Processor -> (Parameterof Processor)))
(define (wrap-processor p)
  (make-parameter p))

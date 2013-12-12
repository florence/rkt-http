#lang typed/racket
;; private bindings for processors
(provide Request-Response FProcessor Processor processor-parameter-wrapper wrap-processor processor-parameter-wrapper-processor)
(require "shared.rkt")


(define-type Request-Response (req -> resp))
(define-type FProcessor (Request-Response -> Request-Response))
(struct: processor-parameter-wrapper ([processor : (Parameterof FProcessor)]))
(: wrap-processor : (FProcessor -> processor-parameter-wrapper))
(define (wrap-processor p)
  (processor-parameter-wrapper (make-parameter p)))

(define-type Processor
  (U processor-parameter-wrapper
     FProcessor))
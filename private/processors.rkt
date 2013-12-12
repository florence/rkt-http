#lang typed/racket
;; private bindings for processors
(provide Processor wrap-processor processor-parameter-wrapper
         processor-parameter-wrapper-processor Request-Response)
(require "shared.rkt")


(define-type Request-Response (req -> resp))
(struct: processor-parameter-wrapper ([processor : (Parameterof Processor)]))
(: wrap-processor : (Processor -> Processor))
(define (wrap-processor p)
  (processor-parameter-wrapper (make-parameter p)))

(define-type Processor
  (U processor-parameter-wrapper
     (Request-Response -> Request-Response)))
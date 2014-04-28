#lang typed/racket
;; private bindings for processors
(provide 
 ;;types
 Request-Response Processor BasicProcessor RawProcessor
 ;;functions
 make-processor
 make-processor/parameter
 flatten-processor-list)

(require "shared.rkt")

(define-type Request-Response (req -> resp))
(define-type RawProcessor (Request-Response -> Request-Response))
(struct basic-proc ([raw : RawProcessor]))
(define-type BasicProcessor basic-proc)
(define-type Processor 
  (U BasicProcessor
     (Parameterof BasicProcessor)))

(: make-processor :
   (case->
    [RawProcessor -> BasicProcessor]
    [(req -> req) (resp -> resp) -> BasicProcessor]))
(define make-processor 
  (case-lambda
    [(p) (basic-proc p)]
    [(req* resp*) 
     (make-processor
      (lambda: ([client : Request-Response])
        (lambda: ([r : req])
          (resp* (client (req* r))))))]))

(: make-processor/parameter : 
   (case->
    [RawProcessor -> (Parameterof BasicProcessor)]
    [(req -> req) (resp -> resp) -> (Parameterof BasicProcessor)]))
(define make-processor/parameter
  (case-lambda 
    [(p) (make-parameter (make-processor p))]
    [(req* resp*) (make-parameter (make-processor req* resp*))]))


(: flatten-processor-list : (Request-Response (Listof Processor) -> Request-Response))
;;flatten the processors, with the first being the bottom of the chain, after base
(define (flatten-processor-list base processors)
  (for/fold ([call base]) ([p processors])
    ((processor->rawprocessor p) call)))

(: processor->rawprocessor : (Processor -> RawProcessor))
(define (processor->rawprocessor p)
  (cond 
   [(basic-proc? p) (basic-proc-raw p)]
   [else (processor->rawprocessor (p))]))

#lang typed/racket
;; private bindings for processors
(provide Request-Response Processor make-processor)
(require "shared.rkt")


(define-type Request-Response (req -> resp))
(define-type Processor
  (Rec P
       (case->
        [-> P]
        [P -> Void]
        [Request-Response Any -> Request-Response])));Warning! Here be hacks

(: make-processor 
   (case->
    [(req -> req) (resp -> resp) -> Processor]
    [(Request-Response -> Request-Response) -> Processor]))
(define undefined? (make-predicate Undefined))
(define make-processor
  (case-lambda
    [(req* resp*)
     (make-processor
      (lambda: ([client : Request-Response])
        (lambda: ([r : req])
          (resp* (client (req* r))))))]
    [(p)
     (letrec:
       ([bound : Processor
         (case-lambda
           [()
            (if (undefined? bound)
                (error 'rkt-http "internal error. please report.")
                bound)]
           [(new)
            (when (not (eq? new bound));Otherwise we get an infinite loop
              (set! p (lambda: ([r->r : Request-Response]) (new r->r (void)))))];Warning! Here be hacks
           [(r->r _) (p r->r)])])
       bound)]))
     
    

  

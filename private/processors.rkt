#lang racket
;; private bindings for processors
(provide processor/c wrap-processor processor-parameter-wrapper?
         processor-parameter-wrapper-processor)
(require "shared.rkt")

(define request-response/c (-> req? resp?))
(struct processor-parameter-wrapper (processor))
(define (wrap-processor p)
  (processor-parameter-wrapper (make-parameter p)))
(define processor/c (or/c processor-parameter-wrapper?
                          (-> request-response/c request-response/c)))
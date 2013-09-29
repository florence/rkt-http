#lang scribble/doc
@(require scribble/manual)
@(require (for-label racket "main.rkt"))

@title[#:style 'toc]{rkt-http}

@table-of-contents[]

@defmodule[rkt-http]

(or/c 'get 'post 'delete 'put 'head)

@defproc[(request/no-process (method method/c)
                             (url string?))
         resp?]

@defproc[(request (method method/c)
                  (url string?)
                  (#:request-map request-map dict? null))
         resp?]

@defproc[(request/processors
          (method method/c)
          (url string?)
          (#:request-map request-map dict? null)
          (#:processors processors (listof middleware/c) default-middleware))
         resp?]


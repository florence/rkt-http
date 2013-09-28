#lang scribble/doc
@(require scribble/manual)
@(require (for-label racket "main.rkt"))

@title[#:style 'toc]{rkt-http}

@table-of-contents[]

@defmodule[rkt-http]

@defproc[(request (method (or/c 'get 'post 'delete 'put 'head))
                  (url string?)
                  (#:request-map request-map dict? null))
         resp?]
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
Makes a request of type method to the given url. Equivlanet to @racket[(request/processor method url empty empty)].
This method is the most basic of http requests, doing no pre or post processing on the request. The response body
will be a string containing the raw results.

@defproc[(request (method method/c)
                  (url string?)
                  (#:request-map request-map dict? empty))
         resp?]
Like @racket[request/processors], except it uses the default list of processors.

@defproc[(request/processors
          (method method/c)
          (url string?)
          (request-map dict?)
          (processors (listof processor/c)))
         resp?]

makes an http request using the given method. Each 

@defthing[processor/c (-> (-> req? resp?) (-> req? resp?))]

@defthing[method/c (or/c 'get 'post 'delete 'put 'head #f)]
Contract for http methods.


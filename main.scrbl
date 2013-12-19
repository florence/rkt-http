#lang scribble/doc
@(require scribble/manual)
@(require (for-label racket "main.rkt"))

@title[#:style 'toc]{http}

@table-of-contents[]

@defmodule[rkt-http]

@defproc[(request/no-process (method Method)
                             (url String))
         resp]{
Makes a request of type method to the given url. Equivalent to @racket[(request method url #:request-map empty #:processors empty)].
This method is the most basic of http requests, doing no pre or post processing on the request. The response body
will be a string containing the raw results.
}

@defproc[(request (method Method)
                  (url String)
                  (#:request-map request-map HashTable (hash))
                  (#:processors processors (Listof Processor) default-processors))
         resp]{
Makes an http request using the given method, to the given url. For each call a @racket[req] structure is created,
and passed down the chain of processors, until it hits the bottom. Then a @racket[resp] structure is created for the response,
which is then passed back up the chain of processors, until it is returned from @racket[request].


@racket[request-map] is a @racket[HashTable] containing parameters for the request. Every element of the map is optional.

The last element in the processor chair, which makes the http call, uses two elements from this map: @racket['body] and 
@racket['headers]. @racket['body] will be turned into a string via display, and used directly. @racket['headers] is a @racket[HashTable]
where the keys are the header fields and the values are the header values. These are also written to the http request via
@racket[display].

Before the @racket[resp] is returned to the bottom of the processor chain the headers @racket[dict] is normalize so that all keys
are lowercased and converted to symbols.
}

@defthing[Processor (-> (-> req resp) (-> req resp))]{
A Processor is used to create the processor chain. Each processor will be given the part of the processor
chair below it, and returns the new function chain with itself on top. A processor will, generally, do its processing on the 
request, call the chain below it, and preform post processing on the response.
}

@defthing[Method (U 'get 'post 'delete 'put 'head #f)]{
Type for http methods. If a method is @racket[#f] it expected to be replace by some processor.
}

@defthing[no-op Processor]{
A processor that does nothing. 
}

The default processor chain will convert xml and json response bodies into xexprs and jsexprs. To disable this behavior simply parameterize
the @racket[body-convert] processor to @racket[no-op]:

@racketblock[
        (parameterize ([body-convert no-op])
          (request 'get "www.racket-lang.org"))]

@defproc[(make-processor 
          (#:req  req (-> req req)  values)
          (#:resp resp (-> resp resp) values))
         Processor]{

Creates a basic processor. @racket[req] handles processing the request, and @racket[resp] handles processing the response.

The @racket[no-op] processor could be implemented as @racket[(make-processor)]. 
}

To parse an new @racket['content-type], combine the @racket[body-convert] processor with @racket[make-processor]:

@racketblock[
             (parameterize ([body-convert
                             (make-processor #:request convert-my-request-body
                                             #:response convert-my-response-body)])
               (request 'get "www.mydomain.com" #hash((body . "my-strange-body-format"))))]

However, some processors can not be written with make-processor. For example, to retry a request on a @racket[420] "Enhance Your Calm"
error from twitter.com after some (un)reasonable wait period, we need something a little more involved:

@racketblock[
             (define ((retry-twitter-10-times chain) a-req)
               (let retry-loop ([count 0] [a-resp (client a-req)])
                 (cond
                   [(and (< count 10) (= 420 (resp-code a-resp)))
                    (sleep 10)
                    (retry-loop (add1 count) (client a-req))]
                   [else a-resp])))
             
             (parameterize ([retry retry-twitter-10-times])
               (request 'post "www.twitter.com" REQUEST-MAP))]
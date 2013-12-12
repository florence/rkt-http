#lang typed/racket
(require/typed/provide net/url
                       [#:struct path/param
                                 ([path  : (U String 'up 'same)]
                                  [param : (Listof String)])]
                       [#:struct url
                                 ([scheme   : (Option String)]
                                  [user     : (Option String)]
                                  [host     : (Option String)]
                                  [port     : (Option Exact-Nonnegative-Integer)]
                                  [path-absolute? : Boolean]
                                  [path     : (Listof path/param)]
                                  [query    : (Listof (cons Symbol (Option String)))]
                                  [fragment : (Option String)])]
                       [string->url (String -> url)])
(require/typed/provide web-server/http/request-structs
                       [#:struct header
                                 ([field : Bytes]
                                  [value : Bytes])])
(require/typed/provide web-server/http/request
                       [read-headers (Input-Port -> (Listof header))])
(require/typed/provide racket/base
                       [hasheq (All (A B) (-> (HashTable A B)))])
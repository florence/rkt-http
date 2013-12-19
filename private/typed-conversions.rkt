#lang typed/racket
(require/typed/provide web-server/http/request-structs
                       [#:struct header
                                 ([field : Bytes]
                                  [value : Bytes])])
(require/typed/provide web-server/http/request
                       [read-headers (Input-Port -> (Listof header))])
(require/typed/provide racket/format [~a (Any * -> String)])

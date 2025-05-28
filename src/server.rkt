#lang racket/base

(require web-server/servlet-env
         web-server/servlet

         "routes.rkt")

(displayln "Server started!")
(serve/servlet app
               #:port 3000
               #:servlet-regexp #rx""
               #:command-line? #t
               #:log-file (current-output-port))
(displayln "Server stopped!")

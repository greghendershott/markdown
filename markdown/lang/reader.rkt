#lang s-exp syntax/module-reader
markdown/lang

#:read read-markdown
#:read-syntax read-markdown-syntax
#:whole-body-readers? #t

(require "../parse.rkt")

(define (read-markdown-syntax source in)
  (datum->syntax #f (read-markdown in)))

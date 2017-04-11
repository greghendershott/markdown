#lang info
(define raco-commands '(("markdown"
                         (submod markdown main)
                         "run markdown"
                         #f)))
(define scribblings '(("markdown.scrbl" () (tool-library))))
(define clean '("compiled" "doc" "doc/markdown"))
(define compile-omit-paths '("MarkdownTest_1.0.3" "test"))

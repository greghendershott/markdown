#lang setup/infotab

(define raco-commands '(("markdown" (submod markdown main) "run markdown" #f)))
(define scribblings '(("docs/markdown.scrbl" () (tool-library))))

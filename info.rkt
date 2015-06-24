#lang setup/infotab
(define version "0.18")
(define collection 'multi)
(define deps '("base"
               "sandbox-lib"
               "scribble-lib"
               "srfi-lite-lib"
               "rackjure"
               ("parsack" "0.4")
               "sexp-diff"))
(define build-deps '("at-exp-lib"
                     "html-lib"
                     "rackunit-lib"
                     "redex-lib"))

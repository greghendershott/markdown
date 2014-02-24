#lang setup/infotab
(define version "0.13")
(define collection 'multi)
(define deps '("base"
               "sandbox-lib"
               "scribble-lib"
               "srfi-lite-lib"
               "rackjure"
               ("parsack" "0.3")
               "sexp-diff"))
(define build-deps '("at-exp-lib"
                     "html-lib"
                     "rackunit-lib"
                     "redex-lib"))

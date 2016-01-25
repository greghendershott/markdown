#lang setup/infotab
(define version "0.22")
(define collection 'multi)
(define deps '("base"
               ["parsack" "0.4"]
               ["racket" "6.0"]
               ["rackjure" "0.9"]
               "sandbox-lib"
               "scribble-lib"
               "sexp-diff"
               "srfi-lite-lib"))
(define build-deps '("at-exp-lib"
                     "html-lib"
                     "rackunit-lib"
                     "redex-lib"))

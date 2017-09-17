#lang setup/infotab
(define version "0.25")
(define collection 'multi)
(define deps '("base"
               ["parsack" "0.4"]
               ["racket" "6.0"]
               ["rackjure" "0.9"]
               "sandbox-lib"
               "scribble-lib"
               "srfi-lite-lib"))
(define build-deps '("at-exp-lib"
                     "html-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "redex-lib"
                     "scribble-doc"
                     "sexp-diff"))
(define test-omit-paths '("MarkdownTest_1.0.3" "test"))

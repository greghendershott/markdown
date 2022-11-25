#lang info
(define license 'BSD-2-Clause)
(define version "0.27")
(define collection 'multi)
(define deps '(["base" #:version "6.3"]
               ["parsack" #:version "0.4"]
               "sandbox-lib"
               "scribble-lib"
               "srfi-lite-lib"
               ["threading-lib" #:version "1.1"]))
(define build-deps '("at-exp-lib"
                     "html-lib"
                     "racket-doc"
                     "rackunit-lib"
                     "redex-lib"
                     "scribble-doc"
                     "sexp-diff-lib"))
(define test-omit-paths '("MarkdownTest_1.0.3" "test"))

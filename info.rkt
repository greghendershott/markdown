#lang setup/infotab
(define version "0.7")
(define collection 'multi)
(define deps (list "rackjure"
                   ;; Parsack: It's being actively developed right now
                   ;; so specify the GitHub URL directly. (Otherwise,
                   ;; the pkg.gracket-lang.org PNR updates only every
                   ;; 24 hours which is too long to wait, especially e.g.
                   ;; for Travis CI.)
                   "github://github.com/stchang/parsack/master"
                   ;; "parsack"
                   ))

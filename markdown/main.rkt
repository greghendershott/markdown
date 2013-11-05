#lang racket

(require "parse.rkt"
         "display-xexpr.rkt")

(provide (all-from-out "parse.rkt")
         (all-from-out "display-xexpr.rkt"))

;; For use as command-line pipe.
(module+ main
  (display "<!DOCTYPE html>")
  (display-xexpr `(html (head () (meta ([charset "utf-8"])))
                        (body () ,@(read-markdown)))))

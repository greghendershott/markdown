#lang racket

(require "parse.rkt"
         "display-xexpr.rkt"
         "toc.rkt")

(provide (all-from-out "parse.rkt")
         (all-from-out "display-xexpr.rkt")
         (all-from-out "toc.rkt")
         markdown->html)

(define (markdown->html md)
  (with-output-to-string
    (lambda ()
      (display "<!DOCTYPE html>")
      (display-xexpr `(html (head () (meta ([charset "utf-8"])))
                            (body () ,@md))))))

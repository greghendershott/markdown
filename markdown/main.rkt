;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket

(require "parse.rkt"
         "display-xexpr.rkt"
         "toc.rkt")

(provide (all-from-out "parse.rkt")
         (all-from-out "display-xexpr.rkt")
         (all-from-out "toc.rkt"))

;; For use as command-line pipe.
(module+ main
  (display "<!DOCTYPE html>")
  (display-xexpr `(html (head () (meta ([charset "utf-8"])))
                        (body () ,@(read-markdown)))))

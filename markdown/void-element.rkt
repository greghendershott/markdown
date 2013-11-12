#lang racket/base

(provide void-element?)

(define (void-element? x)
  ;; http://www.w3.org/TR/html-markup/syntax.html#void-element
  (memq (car x)
        '(area base br col command embed hr img input keygen link
               meta param source track wbr)))

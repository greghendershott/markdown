#lang racket

(require "base.rkt"
         (for-syntax racket
                     "base.rkt"))

(provide (all-from-out "base.rkt")
         module-begin/markdown)

(define-syntax-rule (module-begin/markdown body ...)
  (#%plain-module-begin
   (display (markdown->html '(body ...)))))

;; For use as command-line pipe.
(module+ main
  (display (markdown->html (read-markdown))))

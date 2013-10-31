#lang racket

(require rackjure/threading
         ;;"main.rkt"
         "parse.rkt"
         "display-xexpr.rkt"
         )

;; Test: Compare to static file.

(define test-footnote-prefix 'unit-test) ;fixed, not from (gensym)

(module+ test
  (require rackunit
           racket/runtime-path)

  (define-runtime-path test.md "test/test.md")
  (define xs (with-input-from-file test.md
               (thunk (read-markdown test-footnote-prefix))))

  ;; Reference file. Update this periodically as needed.
  (define-runtime-path test.html "test/test.html")

  (define test.out.html (build-path (find-system-path 'temp-dir)
                                    "test.out.html"))

  (with-output-to-file test.out.html #:exists 'replace
                       (lambda ()
                         (display "<!DOCTYPE html>")
                         (~> `(html (head () (meta ([charset "utf-8"])))
                                    (body () ,@xs))
                             display-xexpr)))

  (check-equal? (system/exit-code (~a "diff " test.html " " test.out.html))
                0))

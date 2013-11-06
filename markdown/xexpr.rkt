#lang racket

(provide normalize
         normalize-xexprs)

(module+ test
  (require rackunit))

;; normalize : xexpr? -> xexpr?
;;
;; Do some recursive normalizations on the xexpr:
;; 1. Append consecutive string? elements in the xexpression.
;; 2. Delete any "" elements left after 1.
;; 3. Delete any trailing spaces in the last element.
;; 4. Splice any (@SPLICE) elements like unquote-splicing.
(define (normalize x)
  (match x
    [`(,tag ,as ,es ...)
     `(,tag ,as ,@(let loop ([es (splice es)]) ;; 4
                   (match es
                     [(list (? string? this) (? string? next) more ...) ;; 1
                      (loop (cons (string-append this next) more))]
                     [(cons "" more)    ;; 2
                      (loop more)]
                     [(cons (pregexp "^(.*?)\\s*$" (list _ this)) '()) ;; 3
                      (match this
                        ["" '()]
                        [_ (cons this '())])]
                     [(cons this more)
                      (cons (normalize this) (loop more))]
                     ['() '()])))]
    [x x]))

(module+ test
  (check-equal? (normalize `(p () "a" "b" "c" "d" "e" (p () "1" "2" "3 ")))
                '(p () "abcde" (p () "123"))))

;; normalize-xexprs : (listof xexpr?) -> (listof xexpr?)
;;
;; Like normalize but for a (listof xexpr?) not just one.
(define (normalize-xexprs xs)
  (match (normalize `(_ () ,@xs))
    [`(_ () ,xs ...) xs]))

;; splice : (listof xexpr?) -> (listof xexpr?)
;;
;; Do the equivalent of ,@ a.k.a. unquote-splicing recursively on all
;; `(@SPLICE es ...)` elements, such that the `es` get lifted/spliced.
(define (splice xs)
  (let loop ([xs xs])
    (match xs
      [`((SPLICE ,es ...) ,more ...) (loop (append es more))]
      [(cons this more)              (cons this (loop more))]
      ['()                           '()])))

(module+ test
  (check-equal? (splice `((p () "A")
                          (SPLICE "a" "b")
                          (p () "B")))
                `((p () "A") "a" "b" (p () "B")))
  (check-equal? (normalize `(p () "a" "b" (SPLICE "c" "d") "e" "f"))
                `(p () "abcdef"))
  (check-equal? (normalize `(p () "a" (SPLICE "b" (SPLICE "c") "d") "e"))
                `(p () "abcde")))

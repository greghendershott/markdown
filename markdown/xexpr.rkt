#lang racket

(provide normalize
         normalize-xexprs)

(module+ test
  (require rackunit))

;; normalize : xexpr? -> xexpr?
;;
;; Do some recursive normalizations on the xexpr:
;; 0. Splice any (SPLICE ...) elements like unquote-splicing.
;; 1. Append consecutive string? elements in the xexpression.
;; 2. Delete any "" elements left after 1.
;; 3. Unless pre element, delete any trailing \\s in the LAST element.
(define (normalize x)
  (match x
    [`(,tag ,as ,es ...)
     `(,tag ,as ,@(normalize-elements tag es))]
    [x x]))

(define pre-level (make-parameter 0))

(define (normalize-elements tag es)
  (parameterize ([pre-level (match tag
                              ['pre (add1 (pre-level))]
                              [_          (pre-level)])])
    (let loop ([es (splice es)]) ;; 0
      (match es
        [(list (? string? this) (? string? next) more ...) ;; 1
         (loop (cons (string-append this next) more))]
        [(cons "" more)    ;; 2
         (loop more)]
        [(cons (? string? this) more)
         (cond [(and (zero? (pre-level))
                     (not (eq? tag 'HTML-COMMENT)))
                (let ([this (match more
                              ['() (string-trim this #:left? #f)] ;; 3
                              [_   this])])
                  (match this
                    [""   (loop more)]
                    [this (cons this (loop more))]))]
               [else
                (cons (expand-tabs this) (loop more))])]
        [(cons this more)
         (cons (normalize this) (loop more))]
        ['() '()]))))

(module+ test
  (check-equal? (normalize `(p () "a" "b" "c" "d" "e" (p () "1" "2" "3 ")))
                '(p () "abcde" (p () "123")))
  (check-equal? (normalize `(p () "empty space at end   "))
                '(p () "empty space at end"))
  (check-equal? (normalize `(p () "   "))
                '(p ()))
  ;; pre elements
  (check-equal? (normalize `(pre () "   x   " "   y   "))
                '(pre () "   x      y   "))
  (check-equal? (normalize `(pre () "foo" " " "\t" "\n" " " "bar"))
                '(pre () "foo     \n bar"))
  (check-equal? (normalize `(pre () (code () "   x   " "   y   ")))
                '(pre () (code () "   x      y   ")))
  (check-equal? (normalize `(pre () (code () "foo" " " "\t" "\n" " " "bar")))
                '(pre () (code () "foo     \n bar"))))

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

(define (expand-tabs s [size 4])
  (list->string
   (let loop ([cs (string->list s)]
              [i 0])
     (match cs
       ['() '()]
       [(cons #\tab more)
        (define spaces (- size (modulo i size)))
        (append (make-list spaces #\space) (loop more (+ i spaces)))]
       [(cons c more)
        (cons c (loop more (add1 i)))]))))

(module+ test
  (check-equal? (expand-tabs "\t4567")     "    4567")
  (check-equal? (expand-tabs "0\t4567")    "0   4567")
  (check-equal? (expand-tabs "01\t4567")   "01  4567")
  (check-equal? (expand-tabs "012\t4567")  "012 4567")
  (check-equal? (expand-tabs "0123\t8")    "0123    8")
  (check-equal? (expand-tabs "01\t45\t89") "01  45  89"))

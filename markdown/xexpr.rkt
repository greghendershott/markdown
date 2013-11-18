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
                ;; TO-DO: Expand tabs to spaces, using tab size 4.
                ;; Not simply regexp-replace \t => "    ". Tab stops.
                ;; Only really necessary to pass markdown test suite
                ;; even though it's N/A for real world browsers.
                (cons this (loop more))])]
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
                '(pre () "foo \t\n bar"))
  (check-equal? (normalize `(pre () (code () "   x   " "   y   ")))
                '(pre () (code () "   x      y   ")))
  (check-equal? (normalize `(pre () (code () "foo" " " "\t" "\n" " " "bar")))
                '(pre () (code () "foo \t\n bar"))))

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

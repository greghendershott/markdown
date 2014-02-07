#lang racket

(require "parsack.rkt")

(provide $entity)

(define $char-entity/dec
  (try (pdo (char #\&)
            (char #\#)
            (x <- (many1 $digit))
            (char #\;)
            (return (string->number (list->string x) 10)))))

(define $char-entity/hex
  (try (pdo (char #\&)
            (char #\#)
            (<or> (char #\x)
                  (char #\X))
            (x <- (many1 $hexDigit))
            (char #\;)
            (return (string->number (list->string x) 16)))))

(define $sym-entity
  (try (pdo (char #\&)
            (x <- (many1 (<or> $letter $digit)))
            (char #\;)
            (return (string->symbol (list->string x))))))

(define $not-entity
  (pdo (char #\&)
       (return "&"))) ;; not 'amp -- act like xexpr

(define $entity
  (<or> $char-entity/dec
        $char-entity/hex
        $sym-entity
        $not-entity))

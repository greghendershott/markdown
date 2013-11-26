#lang at-exp racket

;; Try parsing "random" text to see if parser fails. Why? Every input
;; should parse successfully, even if it parses to itself as plain
;; text. There's really no such thing as a syntax error with markdown
;; -- just plain text that didn't turn out to be markdown.
;;
;; This is unlike randomized redex testing, which tries to produce
;; examples of _valid_ markdown that the parser misunderstands. By
;; contrast, this random testing tries to produce examples of
;; _not valid_ markdown that the parser should parse to plain text
;; rather than terminate with a syntax error.
;;
;; Furthermore, we also want to check for the more fundamental bug of
;; the parser terminating never, or "never" -- taking so long that
;; it's worth investigating if the grammar is wrong.

;; Slow. Put in `slow-test` submodule not `test`.
;; Run using `raco test -s slow-test random-test.rkt
(module slow-test racket
  (require racket/sandbox
           "main.rkt")

  ;; In a previous version of this I used completely random
  ;; characters. However we're much more likely to generate something
  ;; that could break the parser if we randomly choose among "tokens"
  ;; that mean something special in markdown (as well as from plain
  ;; words). This results in something that looks like it could be
  ;; markdown -- both to a human and a parser.
  (define (stair num str)
    (for/list ([n num])
      (for/fold ([s str])([_ n])
        (string-append s str))))
  (define tokens
    (list->vector
    `(,@(stair 2 "\n")
      ,@(stair 2 "\r")
      ,@(stair 2 "\r\n")
      ,@(stair 2 "\n\r")
      ,@(stair 4 " ")
      ,@(stair 4 "=")
      ,@(stair 4 "-")
      ,@(stair 4 "`")
      "\n```"
      "\n     "
      "[" "]" "(" ")"
      "&"
      " < "  "> "
      " 1 < 2 "
      "'" "\""
      "<div>" "</div>"
      "<br />"
      "*ipsum* "
      "**ipsum** "
      "***ipsum*** "
      "_ipsum_ "
      "__ipsum__ "
      "___ipsum___ "
      ,@(for/list ([_ 20]) "lorem ")
      ,@(for/list ([_ 20]) "ipsum "))))
  (define (random-token)
    (vector-ref tokens (random (vector-length tokens))))

  (define (random-doc tokens)
    (for/fold ([s ""])
              ([_ (in-range tokens)])
      (string-append s (random-token))))

  (define (check-doc doc)
    (with-handlers
        ([exn:fail? (lambda (x)
                      (newline)
                      (displayln (exn-message x))
                      (displayln @~a{BEGIN @(make-string 65 #\>)})
                      (displayln doc)
                      (displayln @~a{@(make-string 67 #\<) END}))])
      (call-with-limits 30 #f (thunk (parse-markdown doc)))))

  (define (random-test reps tokens)
    (display @~a{Trying @reps docs with @|tokens| "tokens" each: })
    (flush-output)
    (for ([i reps])
      (when (zero? (modulo i 50))
        (display @~a{@(add1 i) })
        (flush-output))
      (check-doc (random-doc tokens)))
    (newline))

  (random-test 500 300)
  ;; (provide (all-defined-out))
  )

;; (require 'slow-test)

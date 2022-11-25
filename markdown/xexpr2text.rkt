;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require racket/format
         racket/function
         racket/list
         racket/match
         racket/string
         threading
         (only-in xml valid-char?)
         "xexpr.rkt")

(provide xexpr->text
         xexprs->text
         xexpr->slug
         xexprs->slug)

(module+ test
  (require rackunit))

;; Not full markdown, just a "lite" variant for human readers only.
(define (xexpr->text x [block-suffix ""])
  (define (heading? s)
    (memq s '(h1 h2 h3 h4 h5 h6 h7 h8 h9)))
  (define (block? s)
    (memq s '(p div li)))
  (define (->s es) ;convert entities to string
    (apply ~a (map (curryr xexpr->text block-suffix) es)))
  (match (normalize x)
    [`(em            ,_ ,es ...) (~a "_" (->s es) "_")]
    [`(strong        ,_ ,es ...) (~a "**" (->s es) "**")]
    [`(code          ,_ ,es ...) (~a "`" (->s es) "`")]
    [`(,(? heading?) ,_ ,es ...) (~a (->s es) ": ")]
    [`(,(? block?)   ,_ ,es ...) (~a (->s es) block-suffix)]
    [`(,(? symbol?)  ,_ ,es ...) (~a (->s es))]
    [(? string? s) s]
    ['ndash "-"]
    ['mdash "--"]
    ['amp "&"]
    [(or 'lsquo 'rsquo) "'"]
    [(or 'ldquo 'rdquo 'quot) "\""]
    [(? valid-char? c) (integer->char c)]
    [_ ""])) ;; ignore others

(module+ test
  (check-equal? (xexpr->text '(em () "italic"))
                "_italic_")
  (check-equal? (xexpr->text '(em ([class "foo"]) "italic"))
                "_italic_")
  (check-equal? (xexpr->text '(strong () "bold"))
                "**bold**")
  (check-equal? (xexpr->text '(strong ([class "foo"]) "bold"))
                "**bold**")
  (check-equal? (xexpr->text '(em () "italic " (strong () "bold") " italic"))
                "_italic **bold** italic_")
  (check-equal? (xexpr->text '(p () "I am some " (em () "italic") " text"))
                "I am some _italic_ text")
  (check-equal? (xexpr->text '(p ([class "foo"])
                                     "I am some " (em () "italic") " text"))
                "I am some _italic_ text")
  (check-equal? (xexpr->text '(p () "M" amp "Ms" mdash "gotta love 'em"))
                "M&Ms--gotta love 'em")
  (check-equal? (xexpr->text '(div () (p () "Hi.") (p () "Hi.")) "\n")
                "Hi.\nHi.\n\n")
  (check-equal? (xexpr->text '(p () "Hi" #x20 "there"))
                "Hi there"))

(define (xexprs->text xs)
  (xexpr->text `(dummy () ,@xs)))

(module+ test
  (check-equal? (xexprs->text
                 '((em () "italic") " and " (strong () "bold") "."))
                "_italic_ and **bold**."))

(define (xexpr-entities x)
  (match x
    [`(,(? symbol?) ,_ ,es ...) (map xexpr-entities es)]
    [_ x]))

(module+ test
  (check-equal? (xexpr-entities '(p () "a" (p () "b" "c")))
                '("a" ("b" "c"))))

(define (xexpr->slug x)
  (define (char-ok? c)
    (case c
      [(#\space #\newline #\-) #t]
      [else (or (char-alphabetic? c) (char-numeric? c))]))
  (define (maybe->hyphen c)
    (case c
      [(#\space #\newline) #\-]
      [else c]))
  (define (str-join xs)
    (string-join xs ""))
  (~>> x
       normalize
       xexpr-entities
       flatten
       (filter (curry string?))
       str-join
       string->list
       (filter (curry char-ok?))
       (map maybe->hyphen)
       list->string
       string-downcase))

(module+ test
  (check-equal? (xexpr->slug '(em () "italic"))
                "italic")
  (check-equal? (xexpr->slug '(em ([class "foo"]) "italic"))
                "italic")
  (check-equal? (xexpr->slug '(strong () "bold"))
                "bold")
  (check-equal? (xexpr->slug '(strong ([class "foo"]) "bold"))
                "bold")
  (check-equal? (xexpr->slug '(em () "italic " (strong () "bold") " italic"))
                "italic-bold-italic")
  (check-equal? (xexpr->slug '(p () "I am some " (em () "italic") " text"))
                "i-am-some-italic-text")
  (check-equal? (xexpr->slug '(p ([class "foo"])
                                     "I am some " (em () "italic") " text"))
                "i-am-some-italic-text")
  (check-equal? (xexpr->slug '(p () "M" amp "Ms" mdash "gotta love 'em"))
                "mmsgotta-love-em")
  (check-equal? (xexpr->slug '(div () (p () "Hi.") (p () "Hi.")))
                "hihi")
  (check-equal? (xexpr->slug '(p () "Hi" #x20 "there"))
                "hithere"))

(define (xexprs->slug xs)
  (xexpr->slug (append xs)))

(module+ test
  (check-equal? (xexprs->slug
                 '((em () "Italic") " and " (strong () "bold") "."))
                "italic-and-bold"))

(define (escape-double-quotes s)
  (regexp-replace* #rx"\"" s "\\&quot;")) ;need to escape `&` in replace str

(module+ test
  (check-equal? (escape-double-quotes "A \"double quote\" in the string.")
                "A &quot;double quote&quot; in the string."))

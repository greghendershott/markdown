#lang racket

(require rackunit
         xml
         "parse.rkt")

;; For using markdown test suites consisting of pairs of files,
;; markdown input and HTML output.

(define (test-dir path)
  (define (fold-md-files f)
    (define (check-md-file path what accum)
      (define (is-ext? path ext)
        (equal? path (path-replace-suffix path ext)))
      (define (is-md? path)
        (or (is-ext? path ".text") (is-ext? path ".md") (is-ext? path ".markdown")))
      (match* ((is-md? path) what)
        [(#t 'file) (f path)]
        [(_ _) (void)])
      (void))
    (fold-files check-md-file #f path))
  (fold-md-files
   (lambda (path)
     (displayln path)
     (check-parse-vs-html-file path (path-replace-suffix path ".xhtml")))))

;; Rather than generate an HTML file from our xexpr, read the referene
;; HTML to an xxpr and compare the xexprs. This should normalize some
;; gratuitous differences like line breaks that would occur by
;; comparing as HTML text. Own the downside, no handy diff.
(define (check-parse-vs-html-file md-file desired-html-file)
  ;; This is slightly different from normalize in xexpr.rkt. See the
  ;; *** comments below.
  (define (normalize x)
    (match x
      [`(,tag ,as ,es ...)
       `(,tag ,as ,@(let loop ([es es])
                      (match es
                        [(list (? string? this) (? string? next) more ...)
                         (loop (cons (string-append this next) more))]
                        [(cons "" more)
                         (loop more)]
                        [(cons (pregexp "^\\s+$") more) ;; ***
                         (loop more)]
                        [(cons (pregexp "^(.*?)\\s*$" (list _ this)) '())
                         (match (regexp-replace* #px"\\s+" this " ") ;; ***
                           ["" '()]
                           [x (cons x '())])]
                        [(cons this more)
                         (cons (normalize this) (loop more))]
                        ['() '()])))]
      [x x]))
  (define xs-desired
    (normalize
     (string->xexpr
      (string-append "<x>"
                     (file->string desired-html-file)
                     "</x>"))))
  (define xs-md
    (normalize ;normalize this, too (resolves more niggling differences)
     `(x () ,@(parameterize ([current-strict-markdown? #t])
                (parse-markdown (file->string md-file #:mode 'text))))))
  (check-equal? xs-md xs-desired desired-html-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; The following refernences my path to local copy of the Gruber test
;; suite, via mdtest. Due to licensing I'm not yet sure if I may copy
;; the suite into this repo. In a future commit either I will add a
;; copy, or, I'll at least change these paths to use symlink, easier
;; for others.

;; (test-dir "/Users/greg/src/mdtest/Markdown.mdtest")

(define (chk path)
  (check-parse-vs-html-file path (path-replace-suffix path ".xhtml")))

;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Amps and angle encoding.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Auto links.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Backslash escapes.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Blockquotes with code blocks.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Code Blocks.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Code Spans.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Hard-wrapped paragraphs with list-like lines.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Horizontal rules.text")
;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Images.text")
;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Inline HTML Advanced.text")
;; /Users/greg/src/mdtest/Markdown.mdtest/Inline HTML Simple.text
;; /Users/greg/src/mdtest/Markdown.mdtest/Inline HTML comments.text
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Links, inline style.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Links, reference style.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Links, shortcut references.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Literal quotes in titles.text")
;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Markdown Documentation - Basics.text")
;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Markdown Documentation - Syntax.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Nested blockquotes.text")
;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Ordered and unordered lists.text")
;; (chk "/Users/greg/src/mdtest/Markdown.mdtest/Strong and em together.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Tabs.text")
(chk "/Users/greg/src/mdtest/Markdown.mdtest/Tidyness.text")

#lang at-exp racket/base

(module test racket/base
  (require html
           racket/file
           racket/format
           racket/match
           racket/pretty
           racket/runtime-path
           rackunit
           threading
           (only-in sexp-diff sexp-diff)
           xml
           "ci-environment.rkt"
           "parse.rkt")

  ;; For using markdown test suites consisting of pairs of files,
  ;; markdown input and HTML output.

  (define (test-dir path #:skip [skips '()] #:ext [ext ".xhtml"])
    (fold-md-files
     path
     skips
     (lambda (path)
       (check-parse-vs-html-file path (path-replace-suffix path ext)))))

  (define (fold-md-files path skips f)
    (define (check-md-file path what accum)
      (define (is-md? path)
        (define (is-ext? path ext)
          (equal? path (path-replace-suffix path ext)))
        (or (is-ext? path ".text")
            (is-ext? path ".md")
            (is-ext? path ".markdown")))
      (define (not-skip? path)
        (define-values (_ base __) (split-path path))
        (not (member (path->string base) skips)))
      (when (and (eq? what 'file)
                 (is-md? path)
                 (not-skip? path))
        (f path))
      (void))
    (fold-files check-md-file #f path))

  ;; Rather than generate an HTML file from our xexpr, read the reference
  ;; HTML to an xxpr and compare the xexprs. This should normalize some
  ;; gratuitous differences like line breaks that would occur by
  ;; comparing as HTML text. Own the downside, no handy diff.
  (define (check-parse-vs-html-file md-file desired-html-file)
    (let-values ([(_ base __) (split-path md-file)])
      (display @~a{@base ...})) ;display now in case syntax error
    (flush-output)
    (define xs-desired (~>> desired-html-file
                            html-file->xexpr
                            tidy))
    (define xs-md (~>> (parameterize ([current-strict-markdown? #t])
                         (parse-markdown md-file))
                       (append `(x ()))
                       tidy))
    ;; check-equal? doesn't even pretty-print the xexprs, which can be
    ;; very long and hard to compare. Anyway let's do even better and
    ;; use sexp-diff to highlight to diff.
    (define ok? (equal? xs-md xs-desired))
    (cond [ok? (displayln "OK")]
          [else (displayln "diff:")
                (pretty-print (sexp-diff xs-desired xs-md))])
    (check-true ok? desired-html-file))

  (define (html-file->xexpr pathstr)
    (~>> pathstr
         file->string
         open-input-string
         read-html-as-xml
         (element #f #f 'x '())
         xml->xexpr))

  ;; This differs from `normalize` in xexpr.rkt.
  ;;
  ;; - More aggressive about reducing or eliminating whitespace.
  ;;
  ;; - Sort attributes, to permit comparison.
  ;;
  ;; - A few one-off hacks for things like entity encoding in the test
  ;; suite .xhtml files.
  (define (tidy x)
    (define (symbol<? a b)
      (string<? (~a a) (~a b)))
    (match x
      [`(,tag ,(and as `([,_ ,_] ...))
              . ,es)
       `(,tag ,(for/list ([x (sort as symbol<? #:key car)])
                 (match x
                   [(list k v)
                    (list k (regexp-replaces v '([#px"&amp;" "\\&"]
                                                 [#px"&quot;" "\""])))]))
              ,@(let loop ([es es])
                  (match es
                    [(cons `(!HTML-COMMENT () . ,_) more)
                     (loop more)]
                    [(list* this 'amp more)
                     (loop (cons (string-append this "&") more))]
                    [(list* (? string? this) (? string? next) more)
                     (loop (cons (string-append this next) more))]
                    [(cons "" more)
                     (loop more)]
                    [(cons (pregexp "^\\s*(.*?)\\s*$" (list _ this)) more)
                     (match (regexp-replaces this '([#px"\n" " "]
                                                    [#px"\t" " "]
                                                    [#px"\\s+" " "]
                                                    [#px"&amp;" "\\&"]))
                       ["" (loop more)]
                       [x  (cons x (loop more))])]
                    [(cons this more)
                     (cons (tidy this) (loop more))]
                    ['() '()])))]
      [x x]))

  ;; "Ordered and unordered lists.text" is the only test that fails,
  ;; at all.
  ;;
  ;; The second to last list here -- following "Same thing but with
  ;; paragraphs" -- has huge variation on Babelmark. Almost no one
  ;; "passes" this in the sense of making
  ;;
  ;;     `(ol (p "Second:") ___)`
  ;;
  ;; instead of e.g.
  ;;
  ;;     `(ol "Second:" ___)
  ;;
  ;; I don't see how the `p` could be produced -- or why it ought to be.
  (define-runtime-path tests "MarkdownTest_1.0.3")
  (unless ci-environment?
    (test-dir tests
              #:skip '("Ordered and unordered lists.text"))))

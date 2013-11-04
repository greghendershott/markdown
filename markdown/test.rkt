#lang at-exp racket

(require rackjure/threading
         "parse.rkt"
         "display-xexpr.rkt")

(module+ test
  (require rackunit
           racket/runtime-path))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Test: Compare to static file.

(define test-footnote-prefix 'unit-test) ;fixed, not from (gensym)

(module+ test
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression tests

(module+ test
  (define-syntax-rule (check-md x y)
    (check-equal? (parse-markdown x) y))

  ;; https://github.com/greghendershott/markdown/issues/6
  (check-md "_italic with `code` inside it_"
            '((em () "italic with " (code () "code") " inside it")))
  (check-md "_italic with **bold** inside it_"
            '((em () "italic with " (strong () "bold") " inside it")))
  ;; https://github.com/greghendershott/markdown/issues/6
  (check-md "**bold with `code` inside it**"
            '((strong () "bold with " (code () "code") " inside it")))
  (check-md "**bold with _italic_ inside it**"
            '((strong () "bold with " (em () "italic") " inside it")))
  ;; https://github.com/greghendershott/markdown/issues/8
  (check-md "And: [Racket \\[the language\\]](http://www.racket-lang.org/)."
            '("And: "
              (a ([href "http://www.racket-lang.org/"])
                 "Racket [the language]")
              "."))
  (check-md "And: [Racket [the language]](http://www.racket-lang.org/)."
            '("And: "
              (a ([href "http://www.racket-lang.org/"])
                 "Racket [the language]")
              "."))
  (check-md "\\[Not a link\\](nope)"
            '("[Not a link](nope)"))
  ;; https://github.com/greghendershott/markdown/issues/5
  (check-md "[![foo](foo.jpg)](foo.html)"
            '((a ([href "foo.html"])
                 (img ([src "foo.jpg"]
                       [alt "foo"]
                       [title ""])))))
  ;; https://github.com/greghendershott/markdown/issues/5
  (check-md "[<img src=\"foo.jpg\" />](foo.html)"
            '((a ([href "foo.html"])
                 (img ([src "foo.jpg"])))))
  ;; https://github.com/greghendershott/markdown/issues/12
  (check-md "```\ncode block\n```\n<!-- more -->\n"
            '((pre () "code block") (!HTML-COMMENT () " more")))
  ;; https://github.com/greghendershott/markdown/issues/10
  (check-md @~a{These here
                -- should be dashes
                }
            '("These here " ndash " should be dashes"))
  (check-md "---\n"
            '((hr ())))
  (check-md "---hey ho"
            '(mdash "hey ho"))
  ;; https://github.com/greghendershott/markdown/issues/4
  (check-md @~a{    * blah blah
                    * blah blah
                    * blah blah
                
                }
            '((pre () "* blah blah\n* blah blah\n* blah blah")))
  (check-md "** no no **"
            '("** no no **"))
  (check-md "_ no no _"
            '("_ no no _"))
  ;; HTML vs. auto-links: Fight! (Not a specific regression test.)
  (check-md "<http://www.example.com/>"
            '((a ([href "http://www.example.com/"])
                 "http://www.example.com/")))
  (check-md "<img src='foo' />\n"
            '((img ((src "foo")))))
  ;; Bold and italic including nesting. (Not a specific regression test.)
  ;; Note the two spaces at each EOL are intentional!
  (check-md (string-join
             '("_Italic_.  "
               "*Italic*.  "
               "__Bold__.  "
               "**Bold**.  "
               "**Bold with _italic_ inside it**.  "
               "_Italic with **bold** inside it_.  "
               "Should be no ____ italics or bold on this line.  "
               "`I am code`.  "
               )
             "\n")
            '((em () "Italic") "." (br ())
              (em () "Italic") "." (br ())
              (strong () "Bold") "." (br ())
              (strong ()"Bold") "." (br ())
              (strong () "Bold with " (em () "italic") " inside it") "." (br ())
              (em () "Italic with " (strong () "bold") " inside it") "." (br ())
              "Should be no ____ italics or bold on this line." (br ())
              (code () "I am code") "." (br ())))
  ;; https://github.com/greghendershott/markdown/issues/14
  (check-md @~a{Here's a [reflink with 'quotes' in it][].
                
                [reflink with 'quotes' in it]: www.example.com
                }
            '((p ()
                 "Here" rsquo "s a "
                 (a ([href "www.example.com"])
                    "reflink with " lsquo "quotes" rsquo " in it") ".")))
  ;; https://github.com/greghendershott/markdown/issues/15
  (check-md "## Heading **with** _formatting_\n"
            '((h2 () "Heading " (strong () "with") " " (em () "formatting"))))
  ;; https://github.com/greghendershott/markdown/issues/16
  (check-md "**Bold** at line start shouldn't be bullet list.\n\n"
            '((p () (strong () "Bold") " at line start shouldn" rsquo "t be bullet list.")))
  ;; https://github.com/greghendershott/markdown/issues/16
  (check-md "1.23 at line start shouldn't be numbered list.\n\n"
            '((p () "1.23 at line start shouldn" rsquo "t be numbered list.")))
  ;; https://github.com/greghendershott/markdown/issues/18
  (check-md "Blah blah [label](http://www.example.com/two--hyphens.html)."
            '("Blah blah "
              (a ([href "http://www.example.com/two--hyphens.html"])
                 "label")
              "."))
  (check-md "Blah blah ![label](http://www.example.com/two--hyphens.html)."
            '("Blah blah "
              (img ([src "http://www.example.com/two--hyphens.html"]
                    [alt "label"]
                    [title ""]))
              "."))
  ;; https://github.com/greghendershott/markdown/issues/21
  (check-md "<pre>1\n2\n3</pre>"
            '((pre () "1\n2\n3")))
  )

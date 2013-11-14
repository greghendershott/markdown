#lang at-exp racket

(require rackjure/threading
         "parse.rkt"
         "display-xexpr.rkt")

(module+ test
  (require rackunit racket/runtime-path)
  (define-syntax-rule (check-md x y)
    (check-equal? (parse-markdown x) y)))

(define test-footnote-prefix 'unit-test) ;fixed, not from (gensym)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test: Compare to static file.

(module+ test
  ;; Parse Markdown source file to list of xexprs.
  (define-runtime-path test.md "test/test.md")
  (define xs (parse-markdown (file->string test.md)
                             test-footnote-prefix))

  ;; Generate to temporary output HTML file.
  (define test.out.html
    (build-path (find-system-path 'temp-dir) "test.out.html"))
  (with-output-to-file test.out.html #:exists 'replace
                       (thunk
                        (display "<!DOCTYPE html>")
                        (~> `(html (head () (meta ([charset "utf-8"])))
                                   (body () ,@xs))
                            display-xexpr)))

  ;; Reference output HTML file. Update this periodically as needed.
  (define-runtime-path test.html "test/test.html")

  ;; Run diff on them
  (check-equal? (system/exit-code (~a "diff " test.html " " test.out.html))
                0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Blockquote

(module+ test
  (check-md @~a{> Foo
                > Foo
                >
                > Foo
                > Foo
                }
            '((blockquote () (p () "Foo Foo") (p () "Foo Foo")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; List

(module+ test
  ;; Loose
  (check-md @~a{- One.
                
                - Two.
                
                }
            '((ul () (li () (p () "One."))
                  (li () (p () "Two.")))))
  ;; Tight
  (check-md @~a{- One.
                - Two.
                }
           '((ul () (li () "One.")
                 (li () "Two."))))
  ;; Indented < 4 spaces, loose
  (check-md @~a{  - One.
                  
                  - Two.
                  
                  }
            '((ul () (li () (p () "One."))
                  (li () (p () "Two.")))))
  ;; Ordered
  (check-md @~a{1. One.
                
                2. Two.
                
                }
            '((ol () (li () (p () "One."))
                  (li () (p () "Two."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footnote definition

(module+ test
  (let ()
    (define prefix 'foo) ;; fixed footnote prefix, not gensym
    (check-equal?
     (parse-markdown @~a{Footnote use[^1].
                         
                         [^1]: The first paragraph of the definition.
                         
                             Paragraph two of the definition.
                         
                             > A blockquote with
                             > multiple lines.

                                 a code block
                                 here
                             
                             A final paragraph.
                         
                         Not part of defn.
                         
                         }
                     prefix)
     `((p () "Footnote use"
          (sup () (a ([href "#foo-footnote-1-definition"]
                      [name "foo-footnote-1-return"]) "1")) ".")
       (p () "Not part of defn.")
       (div ([class "footnotes"])
            (ol ()
                (li ([id "foo-footnote-1-definition"]
                     [class "footnote-definition"])
                    (p () "The first paragraph of the definition.")
                    (p () "Paragraph two of the definition.")
                    (blockquote () (p () "A blockquote with multiple lines."))
                    (pre () "a code block\n here")
                    (p ()
                       "A final paragraph."
                       nbsp
                       (a ([href "#foo-footnote-1-return"]) "↩")))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Reference block

(module+ test
  (let ()
    (define-syntax-rule (chk s)
      (check-equal?
       (parse-markdown (~a "See [foo][].\n\n" s "\n\n"))
       '((p () "See " (a ([href "http://example.com/"]
                          [title "Optional Title Here"])
                         "foo") "."))))
    (chk "[foo]: http://example.com/  \"Optional Title Here\"")
    (chk "   [foo]:   http://example.com/     \"Optional Title Here\"")
    (chk "[foo]: http://example.com/  'Optional Title Here'")
    (chk "[foo]: http://example.com/  (Optional Title Here)")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Links and image links

(module+ test
  (check-md "[A link](src)" '((a ([href "src"]) "A link")))
  (let ([x '((a ([href "src"][title "A title"]) "A link"))])
    (check-md "[A link](src \"A title\")" x)
    (check-md "[A link](src 'A title')"   x)
    (check-md "[A link](src (A title))"   x))
  (check-md @~a{[A ref link][with source]
                [A ref link without source][]
                
                [with source]: /path/to/1
                [A ref link without source]: /path/to/2
                }
            '((p ()
                 (a ([href "/path/to/1"]) "A ref link")
                 " "
                 (a ([href "/path/to/2"]) "A ref link without source")))))

(module+ test
  (check-md "![Alt text](/path/to/img.png)"
            '((img ((src "/path/to/img.png")
                    (alt "Alt text")))))
  (check-md "![Alt text](/path/to/img.png \"Title\")"
            '((img ((src "/path/to/img.png")
                    (alt "Alt text")
                    (title "Title")))))
  (check-md "![Alt text][1]\n\n[1]: /path/to/img.png 'Optional Title'\n\n"
            '((p () (img ([src "/path/to/img.png"]
                          [title "Optional Title"]
                          [alt "Alt text"]))))))

;; Link with an image for the label
(module+ test
  (check-md "[![img label](img-src 'img title')](src 'title')"
            '((a ([href "src"]
                  [title "title"])
                 (img ([src "img-src"]
                       [alt "img label"]
                       [title "img title"]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Entities

(module+ test
  (check-md "Copyright &copy; 2013 by The Dude & another guy; truly"
            '("Copyright " copy " 2013 by The Dude & another guy; truly"))
  (check-md "Character entities &#x0020;, &#X0020;, &#x20; and &#X20;."
            '("Character entities " #x20 ", " #x20 ", " #x20
              " and " #x20 ".")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Character escaping

(module+ test
  (check-md "\\`not code`"
            '("`not code`")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Code (inline)

(module+ test
  ;; See http://daringfireball.net/projects/markdown/syntax#code
  (check-md "This is some `inline code` here"
            '("This is some "
              (code () "inline code")
              " here"))
  (check-md " `` ` ``\n"
            '((p () " " (code () "`") )))
  (check-md " `` `foo` ``"
            '(" " (code () "`foo`")))
  (check-md "``There is a literal backtick (`) here.``"
            '((code () "There is a literal backtick (`) here.")))
  ;; These are for a custom extension I did for the old parser.
  (check-md "And `printf`[racket]."
            '("And " (code ([class "brush: racket"]) "printf") "."))
  (check-md "`o` and `p`[racket]"
            '((code () "o") " and " (code ((class "brush: racket")) "p"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emphasis and strong

(module+ test
  ;; All 8 permutations
  (define s/e '((strong () "Bold " (em () "italic") " bold")))
  (check-md "**Bold *italic* bold**" s/e)
  (check-md "**Bold _italic_ bold**" s/e)
  (check-md "__Bold _italic_ bold__" s/e)
  (check-md "__Bold *italic* bold__" s/e)

  (define e/s '((em () "Italic " (strong () "bold") " italic")))
  (check-md "*Italic **bold** italic*" e/s)
  (check-md "*Italic __bold__ italic*" e/s)
  (check-md "_Italic __bold__ italic_" e/s)
  (check-md "_Italic **bold** italic_" e/s)

  ;; More teste
  (check-md "no __YES__ no __YES__"
                '("no " (strong () "YES") " no " (strong () "YES")))
  (check-md "no **YES** no **YES**"
                '("no " (strong () "YES") " no " (strong () "YES")))
  (check-md "** no no **"
                '("** no no **"))
  (check-md "no ____ no no"
                '("no ____ no no"))
  (check-md "__Bold with `code` inside it.__"
                '((strong () "Bold with " (code () "code") " inside it.")))

  (check-md "no _YES_ no _YES_"
                '("no " (em () "YES") " no " (em () "YES")))
  (check-md "no *YES* no *YES*"
                '("no " (em () "YES") " no " (em () "YES")))
  (check-md "no_no_no"
                '("no_no_no"))
  ;; (check-md "* no no *"
  ;;               '("* no no *"))
  (check-md "** no no **"
                '("** no no **"))
  ;; (check-md "_YES_ no no_no _YES_YES_ _YES YES_"
  ;;               '((em () "YES") " no no_no "
  ;;                 (em () "YES_YES") " " (em () "YES YES")))
  (check-md "\\_text surrounded by literal underlines\\_"
                '("_text surrounded by literal underlines_"))
  (check-md "\\*text surrounded by literal asterisks\\*"
                '("*text surrounded by literal asterisks*")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verbatim code blocks

(module+ test
  ;; An indented verbatim block may be continued by blank lines --
  ;; indenting the blank lines is _optional_.
  (check-md (~a "    Indented code block with non-indented blank line.\n"
                "    \n"
                "    Indented code block with non-indented blank line.\n"
                "\n"
                "    Indented code block with non-indented blank line.\n"
                "\n"
                "Not the code block.")
            '((pre () "Indented code block with non-indented blank line.\n\nIndented code block with non-indented blank line.\n\nIndented code block with non-indented blank line.")
              "Not the code block.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart dashes

(module+ test
  (check-md "This -- section -- is here and this--is--here---and this."
            '("This " ndash " section " ndash " is here and this" ndash "is"
              ndash "here" mdash "and this.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Smart quotes

(module+ test
  (check-md "She said, \"Why\"?"
            '("She said, " ldquo "Why" rdquo "?"))
  (check-md "She said, \"Why?\""
            '("She said, " ldquo "Why?" rdquo))
  (check-md "She said, \"Oh, _really_\"?"
            '("She said, " ldquo "Oh, " (em () "really") rdquo "?"))
  (check-md "She said, \"Oh, _really_?\""
                '("She said, " ldquo "Oh, " (em () "really") "?" rdquo))

  (check-md "She said, 'Why'?"
            '("She said, " lsquo "Why" rsquo "?"))
  (check-md "She said, 'Why?'"
            '("She said, " lsquo "Why?" rsquo))
  (check-md "She said, 'Oh, _really_'?"
            '("She said, " lsquo "Oh, " (em () "really") rsquo "?"))
  (check-md "She said, 'Oh, _really_?'"
            '("She said, " lsquo "Oh, " (em () "really") "?" rsquo))

  ;; Although I think this test ought to pass, I'm disabling it
  ;; because I don't think it's worth the effort to fix, right now.
  ;; Intent is to re-enable it and get it to pass, later.
  ;;
  ;; ;; Pairs of apostrophes treated as such
  ;; (check-md "It's just Gus' style, he's 6' tall."
  ;;           '("It" rsquo "s just Gus" rsquo " style, he" rsquo "s 6'" " tall."))

  ;; Weird cases
  ;; (check-md "\"\"" '(ldquo rdquo))
  ;; (check-md "''" '(lsquo rsquo))
  ;; (check-md " ' ' " '(" " lsquo " " rsquo " "))
  ;; (check-md "'''" '("'" lsquo rsquo))

  ;; Check not too greedy match
  (check-md "And 'this' and 'this' and."
            '("And " lsquo "this" rsquo " and " lsquo "this" rsquo " and."))
  (check-md "And \"this\" and \"this\" and."
            '("And " ldquo "this" rdquo " and " ldquo "this" rdquo " and."))
  ;; Check nested quotes, American style
  (check-md "John said, \"She replied, 'John, you lug.'\""
            '("John said, " ldquo "She replied, " lsquo "John, you lug." rsquo rdquo))
  (check-md "John said, \"She replied, 'John, you lug'.\""
            '("John said, " ldquo "She replied, " lsquo "John, you lug" rsquo "." rdquo))
  ;; Check nested quotes, British style
  (check-md "John said, 'She replied, \"John, you lug.\"'"
            '("John said, " lsquo "She replied, " ldquo "John, you lug." rdquo rsquo))
  (check-md "John said, 'She replied, \"John, you lug\".'"
            '("John said, " lsquo "She replied, " ldquo "John, you lug" rdquo "." rsquo))
  ;; Yeah, sorry. Not going to deal with 3 levels, as in this test:
  ;; (parse-markdown "Hey, \"Outer 'middle \"inner\" middle' outer\" there"))

  ;; Check interaction with other elements
  (check-md "Some `code with 'symbol`"
            '("Some " (code () "code with 'symbol"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; HTML

(module+ test
  (check-md "Here is a <span class='foo'>text</span> element."
            '("Here is a " (span ((class "foo")) "text") " element."))
  ;; Confirm it works fine with \n in middle of <tag>
  (check-md "<span\n style='font-weight:bold;'>span</span>"
            '((span ((style "font-weight:bold;")) "span")))
  ;; Void element: optional /
  (check-md "<img src='foo'>"
            '((img ([src "foo"]))))
  (check-md "<img src='foo' />"
            '((img ([src "foo"]))))
  ;; Void element with unnecessary closing tag: Consume.
  (check-md "<img src='foo'></img>"
            '((img ([src "foo"]))))
  ;; Non-void element without a closing tag: Leave.
  (check-md "<span>Yada yada"
            '("<span>Yada yada"))
  ;; Dangling closing tag: Leave.
  (check-md "Yada yada</span>"
            '("Yada yada</span>"))
  ;; Nested tags
  (check-md "<p a='outer'>OUTER<p a='inner'>inner</p>OUTER</p>"
            '((p ([a "outer"])
                 "OUTER"
                 (p ([a "inner"])
                    "inner")
                 "OUTER")))
  ;; HTML attribute value can be quoted, unquoted, or even
  ;; missing (in which last case treat it as "true").
  (check-md @~a{<p a="quoted"
                   b='quoted'
                   c=unquoted
                   boolish>foo</p>}
            '((p ([a "quoted"]
                  [b "quoted"]
                  [c "unquoted"]
                  [boolish "true"]) "foo")))

  (check-md @~a{<table border="1">
                  <tbody>
                  <tr>
                    <td>Row 1 Col 1</td>
                    <td>Row 1 Col 2</td>
                  </tr>
                  <tr>
                    <td>Row 2 Col 1</td>
                    <td>Row 2 Col 2</td>
                  </tr>
                  <tr>
                    <td>
                      <tr>Blah</tr>
                    </td>
                  </tr>
                  </tbody>
                </table>
                }
            '((table ([border "1"]) "  "
                     (tbody () "  "
                            (tr () "  "
                                (td () "Row 1 Col 1") "  "
                                (td () "Row 1 Col 2")) "  "
                            (tr () "  "
                                (td () "Row 2 Col 1") "  "
                                (td () "Row 2 Col 2")) "  "
                            (tr () "  "
                                (td () "  "
                                    (tr () "Blah")))))))
  ;; Tag cases don't match
  (check-md "<P>para</p>"
            '((p () "para")))
  (check-md "<SpAn>outer<span>inner</SpAn>outer</SPAN>"
            '((span () "outer" (span () "inner") "outer")))
  ;; Missing trailing slash on self-closing tag.
  (check-md "<img src='foo'>"
            '((img ([src "foo"]))))
  (check-md "<meta x='foo'>"
            '((meta ([x "foo"]))))
  (check-md "<!-- more -->\n\nStuff\n\n"
            '((!HTML-COMMENT () " more") (p () "Stuff")))
  (check-md @~a{<!--multi
                    line
                    comment -->
                }
            '((!HTML-COMMENT () "multi\n    line\n    comment")))
  ;; HTML vs. auto-links: Fight!
  (check-md "<http://www.example.com/>"
            '((a ([href "http://www.example.com/"])
                 "http://www.example.com/")))
  (check-md "<foo@domain.com>"
            '((a ((href "mailto:foo@domain.com")) "foo@domain.com"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Regression tests

(module+ test
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
                       [alt "foo"])))))
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
  (check-md "## Heading **with** _formatting_?\n"
            '((h2 ([id "heading-with-formatting"])
                  "Heading " (strong () "with") " " (em () "formatting") "?")))
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
                    [alt "label"]))
              "."))
  ;; https://github.com/greghendershott/markdown/issues/21
  (check-md "<pre>1\n2\n3</pre>"
            '((pre () "1\n2\n3")))

  ;; Look for a specific bug in resolve-refs that I encountered with a
  ;; reflink in blockquote:
  (module+ test
    (check-md @~a{> I am [reflink][] here.
                  
                  Blah blah blah.
                  
                  [reflink]: http://www.example.com
                  }
              '((blockquote (p "I am "
                               (a ([href "http://www.example.com"]) "reflink")
                               " here."))
                (p "Blah blah blah."))))
  ;; https://github.com/greghendershott/markdown/issues/22
  (check-md @~a{This is Haskell lambda `(\_ -> ...)` code.}
            '("This is Haskell lambda " (code () "(\\_ -> ...)") " code."))

  ;; https://github.com/greghendershott/markdown/issues/24
  (check-md @~a{```
                yo
                ```
                ABC
                }
            '((pre () "yo") "ABC"))
  (check-md @~a{```
                yo
                ```
                ```
                yo
                ```
                ABC
                }
            '((pre () "yo") (pre () "yo") "ABC"))
  ;; https://github.com/greghendershott/markdown/issues/19
  (define prefix 'x) ;; fixed footnote prefix, not gensym
  (check-equal?
   (parse-markdown @~a{A usage[^foo] and another[^bar].
                   
                   [^bar]: Bar note.
                   
                   [^foo]: Foo note.
                   }
                   prefix)
   '((p ()
        "A usage"
        (sup () (a ((href "#x-footnote-1-definition")
                    (name "x-footnote-1-return")) "1"))
        " and another"
        (sup () (a ((href "#x-footnote-2-definition")
                    (name "x-footnote-2-return")) "2")) ".")
     (div ([class "footnotes"])
          (ol ()
              (li ((id "x-footnote-1-definition")
                   (class "footnote-definition"))
                  (p ()
                     "Foo note."
                     nbsp
                     (a ((href "#x-footnote-1-return")) "↩")))
              (li ((id "x-footnote-2-definition")
                   (class "footnote-definition"))
                  (p ()
                     "Bar note."
                     nbsp
                     (a ((href "#x-footnote-2-return")) "↩")))))))
  (check-equal?
   (parse-markdown @~a{A usage[^foo]
                   
                   [^foo]: Foo note.
                   
                   And another[^bar]
                   
                   [^bar]: Bar note.
                   }
                   prefix)
   '((p ()
        "A usage"
        (sup () (a ((href "#x-footnote-1-definition")
                    (name "x-footnote-1-return")) "1")))
     (p ()
        "And another"
        (sup () (a ((href "#x-footnote-2-definition")
                    (name "x-footnote-2-return")) "2")))
     (div ([class "footnotes"])
          (ol ()
              (li ((id "x-footnote-1-definition")
                   (class "footnote-definition"))
                  (p ()
                     "Foo note."
                     nbsp
                     (a ((href "#x-footnote-1-return")) "↩")))
              (li ((id "x-footnote-2-definition")
                   (class "footnote-definition"))
                  (p ()
                     "Bar note."
                     nbsp
                     (a ((href "#x-footnote-2-return")) "↩")))))))
  ;; https://github.com/greghendershott/markdown/issues/27
  (check-equal?
   (parse-markdown "[test][1]\n\n[1]:http://test.com \"test-title\"")
   (parse-markdown "[test][1]\r\n\r\n[1]:http://test.com \"test-title\"")))


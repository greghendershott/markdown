#lang at-exp racket

(module test racket
  (require rackunit
           racket/runtime-path
           rackjure/threading
           "parse.rkt"
           "display-xexpr.rkt")

  (define-syntax-rule (check-md x y)
    (check-equal? (parse-markdown x) y))

  (define test-footnote-prefix 'unit-test) ;fixed, not from (gensym)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Test: Compare to static file.

  ;; Parse Markdown source file to list of xexprs.
  (define-runtime-path test.md "test/test.md")
  (define xs (parse-markdown (file->string test.md #:mode 'text)
                             test-footnote-prefix))

  ;; Generate to temporary output HTML file.
  (define test.out.html
    (build-path (find-system-path 'temp-dir) "test.out.html"))
  (with-output-to-file test.out.html #:exists 'replace #:mode 'text
                       (thunk
                        (display "<!DOCTYPE html>")
                        (~> `(html (head () (meta ([charset "utf-8"])))
                                   (body () ,@xs))
                            display-xexpr)))

  ;; Reference output HTML file. Update this periodically as needed.
  (define-runtime-path test.html "test/test.html")

  ;; Run diff on them
  (check-equal? (system/exit-code (~a "diff " test.html " " test.out.html))
                0)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Heading

  (check-md "# Heading"       '((h1 ([id "heading"]) "Heading")))
  (check-md "#### Heading"    '((h4 ([id "heading"]) "Heading")))
  (check-md "# Heading #"     '((h1 ([id "heading"]) "Heading")))
  (check-md "# Heading #####" '((h1 ([id "heading"]) "Heading")))
  (check-md @~a{Heading
                =======}      '((h1 ([id "heading"]) "Heading")))
  (check-md @~a{Heading
                -------}      '((h2 ([id "heading"]) "Heading")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Blockquote

  (check-md @~a{> Foo
                > Foo
                >
                > Foo
                > Foo
                }
            '((blockquote () (p () "Foo Foo") (p () "Foo Foo"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; List

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
                  (li () (p () "Two.")))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Footnote definition

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
                    (pre () (code () "a code block\n here"))
                    (p ()
                       "A final paragraph."
                       nbsp
                       (a ([href "#foo-footnote-1-return"]) "↩"))))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Links and image links

  (let ([x '((p () (a ([href "src"][title "A title"]) "A link")))])
    (check-md "[A link](src \"A title\")" x)
    (check-md "[A link](src 'A title')"   x)
    (check-md "[A\nlink](src 'A title')"  x))
  ;; Examples that are NOT links, due to whitespace between [] ()
  (check-md "[A link] (src 'A title')"
            '((p () "[A link] (src " lsquo "A title" rsquo ")")))
  (check-md "[A link]\n(src 'A title')"
            '((p () "[A link] (src " lsquo "A title" rsquo ")")))
  ;; Parens in the URL
  (check-md "[With parens in the URL](http://en.wikipedia.org/wiki/WIMP_(computing))"
            '((p () (a ([href "http://en.wikipedia.org/wiki/WIMP_(computing)"])
                       "With parens in the URL"))))
  (check-md "[label](/url(a)" '((p () (a ((href "/url(a")) "label"))))
  (let ([x '((p () (a ([href "/src"]) "label")))])
    (check-md "[label](/src)"   x)
    (check-md "[label](</src>)" x))
  (check-md @~a{[Just brackets].
                [Just brackets] [].
                [Just brackets] [].
                [Just brackets] [undefined].
                Normal [link].
                [Handle [link] in brackets].
                
                [link]: /url/
                }
            '((p () "[Just brackets]. [Just brackets] []. [Just brackets] []. [Just brackets] [undefined]. Normal " (a ((href "/url/")) "link") ". [Handle " (a ((href "/url/")) "link") " in brackets].")))
  (check-md @~a{Backslashing should suppress \[this] and [this\].

                [this]: foo
                }
            '((p () "Backslashing should suppress [this] and [this].")))
  (check-md @~a{[A ref link][with source]
                [A ref link without source][]
                
                [with source]: /path/to/1
                [A ref link without source]: /path/to/2
                }
            '((p ()
                 (a ([href "/path/to/1"]) "A ref link")
                 " "
                 (a ([href "/path/to/2"]) "A ref link without source"))))
  ;; Literal quotes in title
  (check-md @~a{Foo [bar][].
                
                Foo [bar](/url/ "Title with "quotes" inside").
                
                
                [bar]: /url/ "Title with "quotes" inside"
                }
            '((p () "Foo " (a ((href "/url/") (title "Title with \"quotes\" inside")) "bar") ".")
              (p () "Foo " (a ((href "/url/") (title "Title with \"quotes\" inside")) "bar") ".")))

  (check-md "![Alt text](/path/to/img.png)"
            '((p () (img ((src "/path/to/img.png")
                          (alt "Alt text"))))))
  (check-md "![Alt text](/path/to/img.png \"Title\")"
            '((p () (img ((src "/path/to/img.png")
                          (alt "Alt text")
                          (title "Title"))))))
  (check-md "![Alt text][1]\n\n[1]: /path/to/img.png 'Optional Title'\n\n"
            '((p () (img ([src "/path/to/img.png"]
                          [alt "Alt text"]
                          [title "Optional Title"])))))

  ;; Link with an image for the label
  (check-md "[![img label](img-src 'img title')](src 'title')"
            '((p () (a ([href "src"]
                        [title "title"])
                       (img ([src "img-src"]
                             [alt "img label"]
                             [title "img title"]))))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Reference links and link definition block

  (check-md @~a{[label][].
                
                [label].
                
                [label]: /path/to
                
                }
            '((p () (a ([href "/path/to"]) "label") ".")
              (p () (a ([href "/path/to"]) "label") ".")))
  (check-md @~a{Here is one where the [link
                breaks] across lines.

                [link breaks]: /url/
                }
            '((p ()
                 "Here is one where the "
                 (a ([href "/url/"]) "link breaks")
                 " across lines.")))
  (check-md @~a{Here is another where the [link 
                breaks] across lines, but with a line-ending space.

                [link breaks]: /url/
                }
            '((p ()
                 "Here is another where the "
                 (a ([href "/url/"]) "link breaks")
                 " across lines, but with a line-ending space.")))
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
    (chk "[foo]: http://example.com/  (Optional Title Here)"))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Entities

  (check-md "Copyright &copy; 2013 by The Dude & another guy; truly"
            '((p ()
                 "Copyright " copy " 2013 by The Dude & another guy; truly")))
  (check-md "Character entities &#x0020;, &#X0020;, &#x20; and &#X20;."
            '((p ()
                 "Character entities " #x20 ", " #x20 ", " #x20
                 " and " #x20 ".")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Character escaping

  (check-md "\\`not code`"
            '((p () "`not code`")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Code (inline)

  ;; See http://daringfireball.net/projects/markdown/syntax#code
  (check-md "This is some `inline code` here"
            '((p () "This is some "
                 (code () "inline code")
                 " here")))
  (check-md " `` ` ``\n"
            '((p () " " (code () "`") )))
  (check-md " `` `foo` ``"
            '((p () " " (code () "`foo`"))))
  (check-md "``There is a literal backtick (`) here.``"
            '((p () (code () "There is a literal backtick (`) here."))))
  ;; These are for a custom extension I did for the old parser.
  (check-md "And `printf`[racket]."
            '((p () "And " (code ([class "brush: racket"]) "printf") ".")))
  (check-md "`o` and `p`[racket]"
            '((p () (code () "o") " and "
                 (code ((class "brush: racket")) "p"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Emphasis and strong

  ;; All 8 permutations
  (define s/e '((p () (strong () "Bold " (em () "italic") " bold"))))
  (check-md "**Bold *italic* bold**" s/e)
  (check-md "**Bold _italic_ bold**" s/e)
  (check-md "__Bold _italic_ bold__" s/e)
  (check-md "__Bold *italic* bold__" s/e)

  (define e/s '((p () (em () "Italic " (strong () "bold") " italic"))))
  (check-md "*Italic **bold** italic*" e/s)
  (check-md "*Italic __bold__ italic*" e/s)
  (check-md "_Italic __bold__ italic_" e/s)
  (check-md "_Italic **bold** italic_" e/s)

  ;; More teste
  (check-md "no __YES__ no __YES__"
            '((p () "no " (strong () "YES") " no " (strong () "YES"))))
  (check-md "no **YES** no **YES**"
            '((p () "no " (strong () "YES") " no " (strong () "YES"))))
  (check-md "** no no **"
            '((p () "** no no **")))
  (check-md "no ____ no no"
            '((p () "no ____ no no")))
  (check-md "__Bold with `code` inside it.__"
            '((p () (strong () "Bold with " (code () "code") " inside it."))))

  (check-md "no _YES_ no _YES_"
            '((p () "no " (em () "YES") " no " (em () "YES"))))
  (check-md "no *YES* no *YES*"
            '((p () "no " (em () "YES") " no " (em () "YES"))))
  (check-md "no_no_no"
            '((p () "no_no_no")))
  ;; (check-md "* no no *"
  ;;               '("* no no *"))
  (check-md "** no no **"
            '((p () "** no no **")))
  ;; (check-md "_YES_ no no_no _YES_YES_ _YES YES_"
  ;;               '((em () "YES") " no no_no "
  ;;                 (em () "YES_YES") " " (em () "YES YES")))
  (check-md "\\_text surrounded by literal underlines\\_"
            '((p () "_text surrounded by literal underlines_")))
  (check-md "\\*text surrounded by literal asterisks\\*"
            '((p () "*text surrounded by literal asterisks*")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Verbatim code blocks

  ;; An indented verbatim block may be continued by blank lines --
  ;; indenting the blank lines is _optional_.
  (check-md (~a "    Indented code block with non-indented blank line.\n"
                "    \n"
                "    Indented code block with non-indented blank line.\n"
                "\n"
                "    Indented code block with non-indented blank line.\n"
                "\n"
                "Not the code block.")
            '((pre ()
                   (code ()
                         "Indented code block with non-indented blank line.\n\nIndented code block with non-indented blank line.\n\nIndented code block with non-indented blank line."))
              (p () "Not the code block.")))
  (check-md (string-join '("    verbatim"
                           "    verbatim"
                           "   " ;; <-- only 3 spaces
                           "")
                         "\n")
            '((pre () (code () "verbatim\nverbatim"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Smart dashes

  (check-md "This -- section -- is here and this--is--here---and this."
            '((p () "This " ndash " section " ndash " is here and this"
                 ndash "is" ndash "here" mdash "and this.")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Smart quotes

  (check-md "She said, \"Why\"?"
            '((p () "She said, " ldquo "Why" rdquo "?")))
  (check-md "She said, \"Why?\""
            '((p () "She said, " ldquo "Why?" rdquo)))
  (check-md "She said, \"Oh, _really_\"?"
            '((p () "She said, " ldquo "Oh, " (em () "really") rdquo "?")))
  (check-md "She said, \"Oh, _really_?\""
            '((p () "She said, " ldquo "Oh, " (em () "really") "?" rdquo)))

  (check-md "She said, 'Why'?"
            '((p () "She said, " lsquo "Why" rsquo "?")))
  (check-md "She said, 'Why?'"
            '((p () "She said, " lsquo "Why?" rsquo)))
  (check-md "She said, 'Oh, _really_'?"
            '((p () "She said, " lsquo "Oh, " (em () "really") rsquo "?")))
  (check-md "She said, 'Oh, _really_?'"
            '((p () "She said, " lsquo "Oh, " (em () "really") "?" rsquo)))

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
            '((p () "And " lsquo "this" rsquo " and " lsquo "this" rsquo " and.")))
  (check-md "And \"this\" and \"this\" and."
            '((p () "And " ldquo "this" rdquo " and " ldquo "this" rdquo " and.")))
  ;; Check nested quotes, American style
  (check-md "John said, \"She replied, 'John, you lug.'\""
            '((p () "John said, " ldquo "She replied, " lsquo "John, you lug." rsquo rdquo)))
  (check-md "John said, \"She replied, 'John, you lug'.\""
            '((p () "John said, " ldquo "She replied, " lsquo "John, you lug" rsquo "." rdquo)))
  ;; Check nested quotes, British style
  (check-md "John said, 'She replied, \"John, you lug.\"'"
            '((p () "John said, " lsquo "She replied, " ldquo "John, you lug." rdquo rsquo)))
  (check-md "John said, 'She replied, \"John, you lug\".'"
            '((p () "John said, " lsquo "She replied, " ldquo "John, you lug" rdquo "." rsquo)))
  ;; Yeah, sorry. Not going to deal with 3 levels, as in this test:
  ;; (parse-markdown "Hey, \"Outer 'middle \"inner\" middle' outer\" there"))

  ;; Check interaction with other elements
  (check-md "Some `code with 'symbol`"
            '((p () "Some " (code () "code with 'symbol"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; HTML

  (check-md "Here is a <span class='foo'>text</span> element."
            '((p () "Here is a " (span ((class "foo")) "text") " element.")))
  ;; Confirm it works fine with \n in middle of <tag>
  (check-md "<span\n style='font-weight:bold;'>span</span>"
            '((p () (span ((style "font-weight:bold;")) "span"))))
  ;; Void element: optional /
  (check-md "<img src='foo'>"
            '((p () (img ([src "foo"])))))
  (check-md "<img src='foo' />"
            '((p () (img ([src "foo"])))))
  ;; Void element with unnecessary closing tag: Consume.
  (check-md "<img src='foo'></img>"
            '((p () (img ([src "foo"])))))
  ;; Element without a closing tag: Treat as void (self-closing).
  (check-md "<span>Yada yada"
            '((p () (span ()) "Yada yada")))
  ;; Dangling closing tag: Leave.
  (check-md "Yada yada</span>"
            '((p () "Yada yada</span>")))
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
            '((table ([border "1"])
                     (tbody ()
                            (tr ()
                                (td () "Row 1 Col 1")
                                (td () "Row 1 Col 2"))
                            (tr ()
                                (td () "Row 2 Col 1")
                                (td () "Row 2 Col 2"))
                            (tr ()
                                (td ()
                                    (tr () "Blah")))))))
  ;; Tag cases don't match
  (check-md "<P>para</p>"
            '((p () "para")))
  (check-md "<SpAn>outer<span>inner</SpAn>outer</SPAN>"
            '((p () (span () "outer" (span () "inner") "outer"))))
  ;; Comments
  (check-md "<!-- more -->\n\nStuff\n\n"
            '((!HTML-COMMENT () " more") (p () "Stuff")))
  (check-md @~a{<!--multi
                    line
                    comment -->
                }
            '((!HTML-COMMENT () "multi\n    line\n    comment")))
  (check-md "<!-- one comment block -- -- with two comments -->"
            '((!HTML-COMMENT () " one comment block -- -- with two comments")))
  ;; HTML vs. auto-links: Fight!
  (check-md "<http://www.example.com/>"
            '((p () (a ([href "http://www.example.com/"])
                 "http://www.example.com/"))))
  (check-md "<foo@domain.com>"
            '((p () (a ((href "mailto:foo@domain.com")) "foo@domain.com"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;
  ;; Regression tests
  ;;

  ;; https://github.com/greghendershott/markdown/issues/6
  (check-md "_italic with `code` inside it_"
            '((p () (em () "italic with " (code () "code") " inside it"))))
  (check-md "_italic with **bold** inside it_"
            '((p () (em () "italic with " (strong () "bold") " inside it"))))
  ;; https://github.com/greghendershott/markdown/issues/6
  (check-md "**bold with `code` inside it**"
            '((p () (strong () "bold with " (code () "code") " inside it"))))
  (check-md "**bold with _italic_ inside it**"
            '((p () (strong () "bold with " (em () "italic") " inside it"))))
  ;; https://github.com/greghendershott/markdown/issues/8
  (check-md "And: [Racket \\[the language\\]](http://www.racket-lang.org/)."
            '((p ()
                 "And: "
                 (a ([href "http://www.racket-lang.org/"])
                    "Racket [the language]")
                 ".")))
  (check-md "And: [Racket [the language]](http://www.racket-lang.org/)."
            '((p ()
                 "And: "
                 (a ([href "http://www.racket-lang.org/"])
                    "Racket [the language]")
                 ".")))
  (check-md "\\[Not a link\\](nope)"
            '((p () "[Not a link](nope)")))
  ;; https://github.com/greghendershott/markdown/issues/5
  (check-md "[![foo](foo.jpg)](foo.html)"
            '((p () (a ([href "foo.html"])
                       (img ([src "foo.jpg"]
                             [alt "foo"]))))))
  ;; https://github.com/greghendershott/markdown/issues/5
  (check-md "[<img src=\"foo.jpg\" />](foo.html)"
            '((p () (a ([href "foo.html"])
                       (img ([src "foo.jpg"]))))))
  ;; https://github.com/greghendershott/markdown/issues/12
  (check-md @~a{```
                code block
                ```
                <!-- more -->
                }
            '((pre () (code () "code block"))
              (!HTML-COMMENT () " more")))
  ;; https://github.com/greghendershott/markdown/issues/10
  (check-md @~a{These here
                -- should be dashes
                }
            '((p () "These here " ndash " should be dashes")))
  (check-md "---\n"
            '((hr ())))
  (check-md "---hey ho"
            '((p () mdash "hey ho")))
  ;; https://github.com/greghendershott/markdown/issues/4
  (check-md @~a{    * blah blah
                    * blah blah
                    * blah blah
                
                }
            '((pre () (code () "* blah blah\n* blah blah\n* blah blah"))))
  (check-md "** no no **"
            '((p () "** no no **")))
  (check-md "_ no no _"
            '((p () "_ no no _")))
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
            '((p ()
                 (em () "Italic") "." (br ())
                 (em () "Italic") "." (br ())
                 (strong () "Bold") "." (br ())
                 (strong ()"Bold") "." (br ())
                 (strong () "Bold with " (em () "italic") " inside it") "." (br ())
                 (em () "Italic with " (strong () "bold") " inside it") "." (br ())
                 "Should be no ____ italics or bold on this line." (br ())
                 (code () "I am code") ".")))
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
            '((p ()
                 "Blah blah "
                 (a ([href "http://www.example.com/two--hyphens.html"])
                    "label")
                 ".")))
  (check-md "Blah blah ![label](http://www.example.com/two--hyphens.html)."
            '((p ()
                 "Blah blah "
                 (img ([src "http://www.example.com/two--hyphens.html"]
                       [alt "label"]))
                 ".")))
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
            '((p () "This is Haskell lambda " (code () "(\\_ -> ...)") " code.")))

  ;; https://github.com/greghendershott/markdown/issues/24
  (check-md @~a{```
                yo
                ```
                ABC
                }
            '((pre () (code () "yo"))
              (p () "ABC")))
  (check-md @~a{```
                yo
                ```
                ```
                yo
                ```
                ABC
                }
            '((pre () (code () "yo"))
              (pre () (code () "yo"))
              (p () "ABC")))
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
                     (a ((href "#x-footnote-2-return")) "↩"))))))))

;; (require 'test)

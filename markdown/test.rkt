#lang at-exp racket/base

(module test racket/base
  (require racket/file
           racket/format
           racket/function
           racket/match
           racket/runtime-path
           racket/string
           racket/system
           rackjure/threading
           rackunit
           sexp-diff
           "display-xexpr.rkt"
           "parse.rkt")

  (define-check (check-md md expect)
    (let ([actual (parse-markdown md)])
      (unless (equal? actual expect)
        (let ([diff (change-kws (sexp-diff actual expect))])
          (with-check-info
           (['actual actual]
            ['expected expect]
            ['diff diff])
           (fail))))))

  (define (change-kws x)
    (match x
      [(cons #:old more) (cons '#:actual (change-kws more))]
      [(cons #:new more) (cons '#:expected (change-kws more))]
      [(cons this more) (cons (change-kws this) (change-kws more))]
      [x x]))

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
  ;; separated by extra blank lines
  ;; https://github.com/greghendershott/markdown/issues/32
  (check-md "# abc\n\n\n\n## def" '((h1 ([id "abc"]) "abc")
                                    (h2 ([id "def"]) "def")))
  (check-md "# \n\n\n\n## "   '((h1 ((id ""))) (h2 ((id "")))))
  (check-md "# \n\n\n\n\n## " '((h1 ((id ""))) (h2 ((id "")))))
  (check-md "# \n\n\n## "     '((h1 ((id ""))) (h2 ((id "")))))

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
            '((ul ()
                  (li () (p () "One."))
                  (li () (p () "Two.")))))
  ;; Tight
  (check-md @~a{- One.
                - Two.
                }
           '((ul ()
                 (li () "One.")
                 (li () "Two."))))
  ;; Indented < 4 spaces, loose
  (check-md @~a{  - One.

                  - Two.

                  }
            '((ul ()
                  (li () (p () "One."))
                  (li () (p () "Two.")))))
  ;; Ordered
  (check-md @~a{1. One.

                2. Two.

                }
            '((ol ()
                  (li () (p () "One."))
                  (li () (p () "Two.")))))

  ;; Nested lists: using 4 spaces
  (check-md @~a{1. One
                    1. 1a
                2. Two
                    1. 2a
                }
            '((ol ()
                  (li () "One" (ol () (li () "1a")))
                  (li () "Two" (ol () (li () "2a"))))))

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

  ;; Link with an image for the label
  (check-md "[![img label](img-src 'img title')](src 'title')"
            '((p () (a ([href "src"]
                        [title "title"])
                       (img ([src "img-src"]
                             [alt "img label"]
                             [title "img title"]))))))

  ;; Inline image:
  (check-md "An image: ![Alt text](/path/to/img.png)" ;explicit
            '((p () "An image: " (img ((src "/path/to/img.png") (alt "Alt text"))))))
    (check-md "An image: ![Alt text]\n\n[Alt text]: /path/to/img.png" ;ref
            '((p () "An image: " (img ((src "/path/to/img.png") (alt "Alt text"))))))
  ;; Inline image:
  (check-md "![Alt text](/path/to/img.png) and more text." ;explicit
            '((p () (img ((src "/path/to/img.png") (alt "Alt text"))) " and more text.")))
  (check-md "![Alt text] and more text.\n\n[Alt text]: /path/to/img.png" ;ref
            '((p () (img ((src "/path/to/img.png") (alt "Alt text"))) " and more text.")))
  ;; Block image, but strict mode:
  (parameterize ([current-strict-markdown? #t])
    (check-md "![Alt _text_](/path/to/img.png)" ;explicit
              '((p () (img ((src "/path/to/img.png") (alt "Alt _text_")))))))
  (parameterize ([current-strict-markdown? #t])
    (check-md "![Alt _text_]\n\n[Alt _text_]: /path/to/img.png" ;ref
              '((p () (img ((src "/path/to/img.png") (alt "Alt _text_")))))))
  ;; Bloock image, not strict mode: Produce a div, and with caption.
  (parameterize ([current-strict-markdown? #f])
    (check-md "![Alt _text_](/path/to/img.png)" ;explicit
              '((div ((class "figure"))
                     (img ((src "/path/to/img.png") (alt "Alt _text_")))
                     (p ((class "caption")) "Alt " (em () "text"))))))
  (parameterize ([current-strict-markdown? #f])
    (check-md "![Alt _text_]\n\n[Alt _text_]: /path/to/img.png" ;ref
              '((div ((class "figure"))
                     (img ((src "/path/to/img.png") (alt "Alt _text_")))
                     (p ((class "caption")) "Alt " (em () "text"))))))
  ;; Block image should parse links in label
  ;; https://github.com/greghendershott/markdown/issues/31
  (check-md "![[A _label_](/url/)](/png/)"
            '((div ((class "figure"))
                   (img ((src "/png/")
                         (alt "[A _label_](/url/)")))
                   (p ((class "caption"))
                      (a ((href "/url/")) "A " (em () "label"))))))

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

  ;; named
  (check-md "Copyright &copy; 2013 by The Dude & another guy; truly"
            '((p ()
                 "Copyright " copy " 2013 by The Dude & another guy; truly")))
  ;; hex
  (check-md "Character entities &#x0020;, &#X0020;, &#x20; and &#X20;."
            '((p ()
                 "Character entities " #x20 ", " #x20 ", " #x20
                 " and " #x20 ".")))
  ;; dec
  (check-md "Character entities &#0032; and &#32;."
            '((p ()
                 "Character entities " 32 " and " 32 ".")))
  ;; ill-formed entity should result in text, not a parse error
  (check-md "&#x2193" ;missing terminating semicolon
            '((p () "&#x2193")))

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

  ;; More tests
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
  (check-md "A * no no *"
            '((p () "A * no no *")))
  (check-md "A ** no no **"
            '((p () "A ** no no **")))
  ;; (check-md "_YES_ no no_no _YES_YES_ _YES YES_"
  ;;               '((p ()
  ;;                    (em () "YES")
  ;;                    " no no_no "
  ;;                    (em () "YES_YES")
  ;;                    " "
  ;;                    (em () "YES YES"))))
  (check-md "\\_text surrounded by literal underlines\\_"
            '((p () "_text surrounded by literal underlines_")))
  (check-md "\\*text surrounded by literal asterisks\\*"
            '((p () "*text surrounded by literal asterisks*")))
  ;; Strong and bold
  (check-md "***This is strong and em.***"
            '((p () (strong () (em () "This is strong and em.")))))
  (check-md "___This is strong and em.___"
            '((p () (strong () (em () "This is strong and em.")))))
  (check-md "So is ***this*** word."
            '((p () "So is " (strong () (em () "this")) " word.")))
  (check-md "So is ___this___ word."
            '((p () "So is " (strong () (em () "this")) " word.")))
  ;; Parsing other inlines inside
  ;; https://github.com/greghendershott/markdown/issues/30
  (check-md "A *[foo](/url/)*"
          '((p () "A " (em () (a ((href "/url/")) "foo")))))
  ;; Sanity check
  (check-md "snake_case_var"
            '((p () "snake_case_var")))

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

  ;; Do
  (check-md "617-555-1212" '((p () "617" ndash "555" ndash "1212")))
  (check-md "1 + 2 - 5" '((p () "1 + 2 " ndash " 5")))
  (check-md "This -- section -- is here and this--is--here---and this."
            '((p () "This " mdash " section " mdash " is here and this"
                 mdash "is" mdash "here" mdash "and this.")))

  ;; Don't
  (check-md "Some not-dashed text" '((p () "Some not-dashed text")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Smart apostrophe

  (check-md @~a{This can't be wrong.}
            '((p () "This can" rsquo "t be wrong.")))

  ;; interaction with smart quotes
  (check-md @~a{This "can't be" wrong.}
            '((p () "This " ldquo "can" rsquo "t be" rdquo " wrong.")))
  (check-md @~a{This 'can't be' wrong.}
            '((p () "This " lsquo "can" rsquo "t be" rsquo " wrong.")))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Smart quotes

  (check-md @~a{She said, "Why"?}
            '((p () "She said, " ldquo "Why" rdquo "?")))
  (check-md @~a{She said, "Why?"}
            '((p () "She said, " ldquo "Why?" rdquo)))
  (check-md @~a{She said, "Oh, _really_"?}
            '((p () "She said, " ldquo "Oh, " (em () "really") rdquo "?")))
  (check-md @~a{She said, "Oh, _really_?"}
            '((p () "She said, " ldquo "Oh, " (em () "really") "?" rdquo)))

  (check-md @~a{She said, 'Why'?}
            '((p () "She said, " lsquo "Why" rsquo "?")))
  (check-md @~a{She said, 'Why?'}
            '((p () "She said, " lsquo "Why?" rsquo)))
  (check-md @~a{She said, 'Oh, _really_'?}
            '((p () "She said, " lsquo "Oh, " (em () "really") rsquo "?")))
  (check-md @~a{She said, 'Oh, _really_?'}
            '((p () "She said, " lsquo "Oh, " (em () "really") "?" rsquo)))
  ;; #\' used after digits should become &prime;
  (check-md "It's just Gus' style, he's 6' tall."
            '((p () "It" rsquo "s just Gus" rsquo " style, he" rsquo "s 6" prime " tall.")))
  (check-md "It's just Gus' style, he's 6'2\" tall."
            '((p () "It" rsquo "s just Gus" rsquo " style, he" rsquo "s 6" prime "2\" tall.")))
  ;; But not a contraction
  (check-md "It's 2's complement"
            '((p () "It" rsquo "s 2" rsquo "s complement")))

  ;; Weird cases
  ;; (check-md "\"\"" '(ldquo rdquo))
  ;; (check-md "''" '(lsquo rsquo))
  ;; (check-md " ' ' " '(" " lsquo " " rsquo " "))
  ;; (check-md "'''" '("'" lsquo rsquo))

  ;; Check not too greedy match
  (check-md @~a{And 'this' and 'this' and.}
            '((p () "And " lsquo "this" rsquo " and " lsquo "this" rsquo " and.")))
  (check-md @~a{And "this" and "this" and.}
            '((p () "And " ldquo "this" rdquo " and " ldquo "this" rdquo " and.")))
  ;; Check nested quotes, American style
  (check-md @~a{John said, "She replied, 'John, you lug.'"}
            '((p () "John said, " ldquo "She replied, " lsquo "John, you lug." rsquo rdquo)))
  (check-md @~a{John said, "She replied, 'John, you lug'."}
            '((p () "John said, " ldquo "She replied, " lsquo "John, you lug" rsquo "." rdquo)))
  ;; Check nested quotes, British style
  (check-md @~a{John said, 'She replied, "John, you lug."'}
            '((p () "John said, " lsquo "She replied, " ldquo "John, you lug." rdquo rsquo)))
  (check-md @~a{John said, 'She replied, "John, you lug".'}
            '((p () "John said, " lsquo "She replied, " ldquo "John, you lug" rdquo "." rsquo)))
  ;; Nested double single double
  (check-md @~a{Hey, "Outer 'middle "inner" middle' outer" there}
            '((p () "Hey, " ldquo "Outer " lsquo "middle " ldquo "inner" rdquo
                 " middle" rsquo " outer" rdquo " there")))

  (check-md @~a{("double quotes in parens") ('single quotes in parens')}
            '((p () "(" ldquo "double quotes in parens" rdquo
                 ") (" lsquo "single quotes in parens" rsquo ")")))

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
  ;; Nested tags
  (check-md "<div a='outer'>OUTER<div a='inner'>inner</div>OUTER</div>"
            '((div ([a "outer"])
                   "OUTER"
                   (div ([a "inner"])
                        "inner")
                   "OUTER")))
  (check-md @~a{<p a="quoted"
                   b='quoted'
                   c=unquoted
                   boolish>foo</p>}
            '((p ([a "quoted"]
                  [b "quoted"]
                  [c "unquoted"]
                  [boolish "boolish"]) "foo")))

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
                                (td () "Row 1 Col 1") " "
                                (td () "Row 1 Col 2")) " "
                            (tr ()
                                (td () "Row 2 Col 1") " "
                                (td () "Row 2 Col 2")) " "
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
  (check-md "<https://www.example.com/>"
            '((p () (a ([href "https://www.example.com/"])
                 "https://www.example.com/"))))
  (check-md "<foo@domain.com>"
            '((p () (a ((href "mailto:foo@domain.com")) "foo@domain.com"))))
  (check-md "<em>foo</em>bar@this<i>yo</i>"
            '((p () (em () "foo") "bar@this" (i () "yo"))))
  ;; Spacing
  (check-md "<p>Here is a <a href='/'>link</a> to check spacing</p>"
            '((p () "Here is a " (a ([href "/"]) "link") " to check spacing")))
  (check-md "This is some <i>really **cool** text</i> you know"
            '((p () "This is some " (i () "really " (strong () "cool") " text") " you know")))
  ;; Optional closing tags
  (check-md "<div><p>a<p>b</p><p>c</div>"
            '((div () (p () "a") (p () "b") (p () "c"))))
  ;; Bad nesting with lj-cut
  (check-md "<lj-cut><p>foo</lj-cut></p>"
            '((!HTML-COMMENT () " more") (p () "foo")))
  ;; Parsing of tags
  (check-md "<p><pre>x</pre></p>"
            '((p () (pre () "x"))))
  ;; In an markdown inline context, what happens to a block element like <table>
  (check-md "<i>x</i><table>y</table>"
            '((p () (i () "x") (table () "y"))))

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
            '((p () "These here " mdash " should be dashes")))
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
                     (a ((href "#x-footnote-2-return")) "↩")))))))
  ;; Unlogged issue with hyphen in HTML attribute name
  (check-md @~a{<script async class="speakerdeck-embed" data-id="ec1069c0e8d10130d58342aa3a8e614d" data-ratio="1.33333333333333" src="//speakerdeck.com/assets/embed.js"></script>}
            '((script ((async "async") (class "speakerdeck-embed") (data-id "ec1069c0e8d10130d58342aa3a8e614d") (data-ratio "1.33333333333333") (src "//speakerdeck.com/assets/embed.js")))))
  ;; https://github.com/greghendershott/markdown/issues/29
  ;; case-insensitive reference IDs
  (check-md "[foo][A] and [foo][a].\n\n[a]: /url/"
            '((p ()
                 (a ((href "/url/")) "foo")
                 " and "
                 (a ((href "/url/")) "foo")
                 ".")))
  (check-md @~a{[Foo _and_ **foo**][] and [FOO _AND_ **FOO**][].

                [foo _aNd_ **FoO**]: /url/}
            '((p
               ()
               (a ((href "/url/")) "Foo " (em () "and") " " (strong () "foo"))
               " and "
               (a ((href "/url/")) "FOO " (em () "AND") " " (strong () "FOO"))
               ".")))
  (check-md "![foo][A] and ![foo][a].\n\n[a]: /url/"
            '((p ()
                 (img ((src "/url/") (alt "foo")))
                 " and "
                 (img ((src "/url/") (alt "foo")))
                 ".")))
  ;; https://github.com/greghendershott/markdown/issues/39
  ;; Close fence: allow space between the ``` and the \n.
  (check-md "```\nCODEBLOCK\n```    \n"
            '((pre () (code () "CODEBLOCK"))))
  ;; https://github.com/greghendershott/markdown/issues/39
  ;; While the hood is open: Also make sure open fence with only
  ;; spaces is trimmed and doesn't end up being a lang named "  ".
  (check-md "```      \nCODEBLOCK\n```    \n"
            '((pre () (code () "CODEBLOCK"))))
  ;; https://github.com/greghendershott/markdown/issues/42
  (check-md "```racket\n`(foo)\n```"
            '((pre ((class "brush: racket")) (code () "`(foo)"))))
  ;; https://github.com/greghendershott/markdown/issues/45
  (check-md @~a{"'Reductive.'" Yes.}
            '((p () ldquo lsquo "Reductive." rsquo rdquo " Yes.")))
  ;; https://github.com/greghendershott/markdown/issues/47
  ;; https://github.com/greghendershott/frog/issues/106
  (check-md "1. <!-- more -->"
            '((ol () (li () (!HTML-COMMENT () " more")))))
  (check-md @~a{1. Blah
                ```
                3
                ```
                B
                <!-- more -->}
            '((ol ()
                  (li ()
                      (p () "Blah " (code () "3") " B")
                      (!HTML-COMMENT () " more")))))
  ;; https://github.com/greghendershott/markdown/issues/52
  (check-md "\\\\(ax^2 + bx + c = 0\\\\)"
            '((p () (script ((type "math/tex"))
                            "ax^2 + bx + c = 0"))))
  (check-md "\\\\[ax^2 + bx + c = 0\\\\]"
            '((p () (script ((type "math/tex; mode=display"))
                            "ax^2 + bx + c = 0"))))
  ;; but single \ still escapes as usual and the contents are still
  ;; parsed as markdown:
  (check-md "\\(some *italic* text\\)"
            '((p () "(some " (em () "italic") " text)")))
  (check-md "\\[some *italic* text\\]"
            '((p () "[some " (em () "italic") " text]")))
  ;; https://github.com/greghendershott/markdown/issues/57
  (check-equal?
   (parse-markdown @~a{Foo [^def].

                       [^def]: Definition of footnote def.


                       aaaaa}
                   'prefix)
   '((p ()
        "Foo "
        (sup ()
             (a ((href "#prefix-footnote-1-definition")
                 (name "prefix-footnote-1-return"))
                "1"))
        ".")
     (p () "aaaaa")
     (div ((class "footnotes"))
          (ol ()
              (li ((id "prefix-footnote-1-definition")
                   (class "footnote-definition"))
                  (p ()
                     "Definition of footnote def."
                     nbsp
                     (a ((href "#prefix-footnote-1-return")) "↩")))))))
  ;; https://github.com/greghendershott/markdown/issues/60
  (check-md "[label](<sou rce>)"
            '((p () (a ([href "sou rce"])
                       "label"))))
  (check-md "[label](<sou rce> \"title\")"
            '((p () (a ([href "sou rce"]
                        [title "title"])
                       "label"))))
  (check-md "![label](<sou rce>)"
            '((div ([class "figure"])
                   (img ([src "sou rce"]
                         [alt "label"]))
                   (p ([class "caption"]) "label"))))
  (check-md "![label](<sou rce> \"title\")"
            '((div ([class "figure"])
                   (img ([src "sou rce"]
                         [alt "label"]
                         [title "title"]))
                   (p ([class "caption"]) "label"))))
  ;; https://github.com/greghendershott/markdown/issues/63
  (check-md "hello\n\nworld!"
            '((p () "hello") (p () "world!")))
  (check-md "hello\r\n\r\nworld!"
            '((p () "hello") (p () "world!")))
  ;; https://github.com/greghendershott/markdown/issues/69
  (check-md @~a{<details>
                ```racket
                (define (twice x)
                  (* 2 x))
                ```
                </details>}
            '((details ()
                       (pre ((class "brush: racket"))
                            (code ()
                                  "(define (twice x)\n  (* 2 x))")))))
  (check-md @~a{<details>
                <summary>Some code</summary>
                ```racket
                (define (twice x)
                  (* 2 x))
                ```
                </details>}
            '((details ()
                       (summary () "Some code")
                       (pre ((class "brush: racket"))
                            (code ()
                                  "(define (twice x)\n  (* 2 x))"))))))

;; (require 'test)

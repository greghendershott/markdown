#lang racket

(require "parsack.rkt"
         "entity.rkt")

;; Note: I would have loved to reuse the work of Racket's
;; read-html-as-xml or the html-parsing package. It's possible to
;; jerry-rig them into a Parsack-style parser -- I tried. However both
;; of those presume you want to parse elements, plural. I need
;; something that parses AT MOST one element, then stops. Anyway,
;; Parsack is pleasant to use, so we'll use that here, too.

(provide (rename-out [$element $html-element]
                     [$block-element $html-block-element]
                     [$not-block-element $html-not-block-element]
                     [$inline-element $html-inline-element]
                     [$comment $html-comment]
                     [$document $html-document]))

(module+ test
  (require rackunit)
  ;; Some syntax to make tests more concise.
  ;; Slightly complicated only because want correct srcloc for fail msgs.
  (define-syntax (with-parser stx)
    (syntax-case stx ()
      [(_ parser [input expected] ...)
       #'(begin (ce parser input expected) ...)]))
  (define-syntax (ce stx)
    (syntax-case stx ()
      [(_ parser input expected)
       (syntax/loc #'input ;this has the desired srcloc
         (check-equal? (parse-result parser input) expected))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define space-chars " \t")
(define $space-char
  (<?> (oneOf space-chars) "space or tab"))

(define $sp
  (<?> (many $space-char)
       "zero or more spaces or tabs"))

(define $spnl
  (<?> (pdo $sp (optional (char #\return)) (optional $newline) $sp
            (return null))
       "zero or more spaces, and optional newline plus zero or more spaces"))

(define (quoted c)
  (try (>>= (between (char c)
                     (char c)
                     (many (noneOf (make-string 1 c))))
            (compose1 return list->string))))
(define $single-quoted (quoted #\'))
(define $double-quoted (quoted #\"))
(define $quoted (<or> $single-quoted $double-quoted))

;; Parsack's <or> disallows zero elements, and `choice` uses it. So:
(define choice*
  (match-lambda
   [(list) $err]
   [(list xs ...) (choice xs)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define list->symbol (compose1 string->symbol list->string))
(define list->tagsym (compose1 string->symbol string-downcase list->string))

(define $attribute
  (<?> (try
        (pdo $spnl
             (key <- (>>= (many1 (noneOf "=>/\n\t "))
                          (compose1 return list->symbol)))
             (val <- (option (symbol->string key)
                             (try
                              (pdo $spnl
                                   (char #\=)
                                   $spnl
                                   (<or> $quoted
                                         (>>= (many1 (noneOf ">/\n\t "))
                                              (compose1 return list->string)))))))
             $spnl
             (return (list key val))))
       "attribute"))

(module+ test
  (with-parser $attribute
    [" k " '(k "k")]
    [" k = 1" '(k "1")]
    [" k = '1'" '(k "1")]
    [" k = \"1\"" '(k "1")]))

(define (open-tag* name-parser end-parser msg)
  (<?> (try (pdo (char #\<)
                 (notFollowedBy (char #\/))
                 (name <- name-parser)
                 (attribs <- (<or> (try (pdo $spnl end-parser (return '())))
                                   (many1Till $attribute end-parser)))
                 (return (list (list->tagsym name)
                               attribs))))
       msg))

(define $any-open-tag
  (open-tag* (many1 (noneOf " />\n"))  (char #\>)    "any open tag"))
(define (open-tag name)
  (open-tag* (stringAnyCase (~a name)) (char #\>)    (format "<~a>" name)))
(define $any-void-tag
  (open-tag* (many1 (noneOf " />\n"))  (string "/>") "any void tag"))
(define (void-tag name)
  (open-tag* (stringAnyCase (~a name)) (string "/>") (format "<~a/>" name)))
(define $any-open-or-void-tag
  (<or> $any-open-tag $any-void-tag))

(module+ test
  (with-parser $any-open-tag
    ["<foo>" '(foo ())]
    ["<foo a = 1 b>" '(foo ([a "1"][b "b"]))]
    ["<foo a='1' b='2'>" '(foo ([a "1"][b "2"]))]
    ["<foo a=1 b=2>" '(foo ([a "1"][b "2"]))]
    ["<p><i b=2></i></p>" '(p ())]))

(module+ test
  (with-parser (open-tag 'foo)
    ["<foo>" '(foo ())]
    ["<foo a = 1 b>" '(foo ([a "1"][b "b"]))]
    ["<foo a='1' b='2'>" '(foo ([a "1"][b "2"]))]
    ["<foo a=1 b=2>" '(foo ([a "1"][b "2"]))]))

(module+ test
  (with-parser $any-void-tag
    ["<foo/>" '(foo ())]
    ["<foo />" '(foo ())]
    ["<foo a = 1 b/>" '(foo ([a "1"][b "b"]))]
    ["<foo a = 1 b />" '(foo ([a "1"][b "b"]))]
    ["<foo a='1' b='2'/>" '(foo ([a "1"][b "2"]))]
    ["<foo a='1' b='2' />" '(foo ([a "1"][b "2"]))]
    ["<foo a=1 b=2/>" '(foo ([a "1"][b "2"]))]
    ["<foo a=1 b=2 />" '(foo ([a "1"][b "2"]))]))

(module+ test
  (with-parser (void-tag 'foo)
    ["<foo/>" '(foo ())]
    ["<foo />" '(foo ())]
    ["<foo a = 1 b/>" '(foo ([a "1"][b "b"]))]
    ["<foo a = 1 b />" '(foo ([a "1"][b "b"]))]
    ["<foo a='1' b='2'/>" '(foo ([a "1"][b "2"]))]
    ["<foo a='1' b='2' />" '(foo ([a "1"][b "2"]))]
    ["<foo a=1 b=2/>" '(foo ([a "1"][b "2"]))]
    ["<foo a=1 b=2 />" '(foo ([a "1"][b "2"]))]))

(define (close-tag* name-parser msg)
  (<?> (try (pdo (char #\<) (char #\/)
                 $spnl (name <- name-parser) $spnl
                 (char #\>)
                 (return (list->tagsym name))))
       msg))

(define $any-close-tag
  (close-tag* (many1 (noneOf " >\n"))   "any close tag"))
(define (close-tag name)
  (close-tag* (stringAnyCase (~a name)) (format "</~a>" name)))

(module+ test
  (with-parser $any-close-tag
    ["</foo>" 'foo]
    ["</FOO>" 'foo]
    ["</foo >" 'foo]))

(module+ test
  (with-parser (close-tag 'foo)
    ["</foo>" 'foo]
    ["</FOO>" 'foo]
    ["</foo >" 'foo])
  (check-exn exn:fail? (lambda () (parse-result (close-tag 'foo) "</bar>"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (element name)
  (try (pdo (open <- (open-tag name))
            $spnl ;; eat leading ws; $content must handle trailing
            (xs <- (manyTill $content (close-tag name)))
            (return (append open xs)))))

(define $other-element
  (try (pdo (open <- $any-open-tag)
            (name <- (return (car open)))
            $spnl ;; eat leading ws; $content must handle trailing
            (xs <- (manyTill $content (close-tag name)))
            (return (append open xs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $junk
  (<?> (>> (many1 (oneOf " \r\n\r"))
           (return ""))
       "whitespace between elements"))

;; Some elements have no content, and we will accept any of:
;; 1. <img .../>
;; 2. <img></img>
;; 3. <img ...>
(define (empty name)
  (<or> (void-tag name) ;1
        (try (pdo (open <- (open-tag name)) ;2
                  (optional $junk)
                  (close-tag name)
                  (return open)))
        (open-tag name))) ;2

(define $hr  (empty 'hr))
(define $img (empty 'img))
(define $meta (empty 'meta))

(module+ test
  (let ([hr '(hr ())]
        [hr/a '(hr ([a "1"]))])
    (with-parser $hr
      ["<hr>" hr]
      ["<hr/>" hr]
      ["<hr />" hr]
      ["<hr></hr>" hr]
      ["<hr a=1>" hr/a]
      ["<hr a=1/>" hr/a]
      ["<hr a=1 />" hr/a]
      ["<hr a=1></hr>" hr/a])))

;; Some elements may be ended by any of the following:
;; 1. A close tag, as usual. e.g. </li>
;; 2. Another open tag of the same e.g. <li> or other (see <p>).
;; 3. A parent end tag. e.g. </ul> or </ol>
;; ;; http://www.w3.org/html/wg/drafts/html/master/syntax.html#syntax-tag-omission
(define (flexi name starters closers)
  (try
   (pdo (open <- (open-tag name))
        $spnl ;; eat leading ws; $content must handle trailing
        (xs <- (manyUntil $content
                          (<or> (close-tag name)
                                (lookAhead
                                 (<or> (choice* (map open-tag  starters))
                                       (choice* (map close-tag closers)))))))
        (return (append open xs)))))

;; A p element's end tag may be omitted if the p element is
;; immediately followed by an address, article, aside, blockquote,
;; div, dl, fieldset, footer, form, h1, h2, h3, h4, h5, h6, header,
;; hgroup, hr, main, menu, nav, ol, p, pre, section, table, or ul,
;; element, or if there is no more content in the parent element and
;; the parent element is not an a element.
(define $p  (flexi 'p
                   '(address article aside blockquote div dl fieldset
                     footer form h1 h2 h3 h4 h5 h6 header hgroup hr
                     main menu nav ol p pre section table ul)
                   '(div td)))
(module+ test
  (with-parser $p
    ["<p>foo</p>" '(p () "foo")]
    ["<p>foo<p>" '(p () "foo")]
    ["<p>foo<p>bar</p>" '(p () "foo")]
    ["<p>foo<h1>" '(p () "foo")]
    ["<p>foo</div>" '(p () "foo")]
    ["<p>foo</td>" '(p () "foo")]
    ["<p>foo<blockquote>" '(p () "foo")]))

;; A thead element's end tag may be omitted if the thead element is
;; immediately followed by a tbody or tfoot element.
(define $thead (flexi 'thead '(tbody tfoot) '(table)))
;; A tfoot element's end tag may be omitted if the tfoot element is
;; immediately followed by a tbody element, or if there is no more
;; content in the parent element.
(define $tfoot (flexi 'tfoot '(tbody) '(table)))
;; A tr element's end tag may be omitted if the tr element is
;; immediately followed by another tr element, or if there is no more
;; content in the parent element.
(define $tr (flexi 'tr '(tr) '(table)))
;; A td element's end tag may be omitted if the td element is
;; immediately followed by a td or th element, or if there is no more
;; content in the parent element.
(define $td (flexi 'td '(td th) '(tr table)))
;; A th element's end tag may be omitted if the th element is
;; immediately followed by a td or th element, or if there is no more
;; content in the parent element.
(define $th (flexi 'th '(td th) '(tr table)))

;; A tbody element's start tag may be omitted if the first thing
;; inside the tbody element is a tr element, and if the element is not
;; immediately preceded by a tbody, thead, or tfoot element whose end
;; tag has been omitted. (It can't be omitted if the element is
;; empty.)
(define $tbody 
  ;; This doesn't attempt to fully implement the above description.
  (<or> (element 'tbody)
        $tr))
        
(module+ test
  (with-parser $tbody
    ["<tbody>foo</tbody>" '(tbody () "foo")]
    ["<tr>foo</tr>" '(tr () "foo")]))

;; Some elements may only contain certain other elements (directly).
(define (only-kids name kids)
  (try (pdo (open <- (open-tag name))
            $spnl ;; eat leading ws; $content must handle trailing
            (xs <- (manyTill (choice* kids) (close-tag name)))
            (return (append open xs)))))

(define $li (flexi 'li '(li) '(ol ul)))
(define $ul (only-kids 'ul (list $li $junk)))
(define $ol (only-kids 'ol (list $li $junk)))

(define $table (only-kids 'table (list $thead $tbody $tfoot $tr $junk)))

(define $comment
  (<?> (try (pdo (string "<!--")
                 (xs <- (many1Till $anyChar (try (string "-->"))))
                 (return `(!HTML-COMMENT () ,(list->string xs)))))
       "<!-- comment -->"))

(define (plain-body tag)
  (<?> (try (pdo (open <- (open-tag tag))
                 (cs <- (manyTill $anyChar (close-tag tag)))
                 (return (append open (list (list->string cs))))))
       "<pre>"))

(define $pre    (plain-body 'pre))
(define $style  (plain-body 'style))
(define $script (plain-body 'script))

(module+ test
  (with-parser $pre
    ["<pre>One\nTwo\nThree</pre>" '(pre () "One\nTwo\nThree")]))

(module+ test
  (with-parser $script
    ["<script>\nif 1 < 2; // <foo>\n</script>"
     '(script () "\nif 1 < 2; // <foo>\n")]))

(module+ test
  (with-parser $style
    ["<style>\ncls {key: value;} /* <foo> */\n</style>"
     '(style () "\ncls {key: value;} /* <foo> */\n")]))

(define $div (element 'div))

;; The strategy here is to define parsers for some specific known
;; elements with special rules, and handle "all other" elements with
;; the "generic" parsers `$any-void-tag` and `$other-element`.
;;
;; Note that some specific element parsers aren't in this list
;; directly. Prime exammple: $table uses quite a few parsers for child
;; elements, which don't _need_ to be here. (And _shouldn't_ be here,
;; unless we were trying to be an extremely tolerant/pragmatic HTML
;; parser like `html-parsing`. But different motivation for this
;; parser.)
(define $element
  (<or> $div
        $p
        $ul
        $ol
        $hr
        $pre
        $script
        $style
        $comment
        $img
        $meta
        $table
        $any-void-tag
        $other-element))

(define $elements
  (many (<or> $element $junk)))

(define $block-element
  (<?> (<or> $comment
             (pdo (open <- (lookAhead $any-open-or-void-tag))
                  (cond [(set-member? block-elements (car open)) $element]
                        [else $err])))
       "block element"))

;; In some cases (such as parsing markdown), the desired concept isn't
;; "inline" so much as it is "not block". For example, this will parse
;; any elements that we don't specifically know about (not in either
;; of the block nor inline sets). Ergo this:
(define $not-block-element
  (<?> (<or> $comment
             (pdo (open <- (lookAhead $any-open-or-void-tag))
                  (cond [(or (not (set-member? block-elements (car open)))
                             (set-member? inline-elements (car open)))
                         $element]
                        [else $err])))
       "not block element"))

(define $inline-element
  (<?> (<or> $comment
             (pdo (open <- (lookAhead $any-open-or-void-tag))
                  (cond [(set-member? inline-elements (car open)) $element]
                        [else $err])))
       "inline element"))

(module+ test
  (check-equal? (parse-result $block-element "<p>foo</p>") '(p () "foo"))
  (check-exn exn:fail? (lambda () (parse-result $block-element "<i>foo</i>")))
  (check-equal? (parse-result $inline-element "<i>foo</i>") '(i () "foo"))
  (check-exn exn:fail? (lambda () (parse-result $inline-element "<p>foo</p>"))))

(define block-elements
  (apply seteq '(!HTML-COMMENT
                 address
                 applet
                 article
                 blockquote
                 body  ; ~= block; useful for markdown
                 button
                 canvas
                 center
                 del
                 dir
                 div
                 dl
                 fieldset
                 figcaption
                 figure
                 footer
                 form
                 h1
                 h2
                 h3
                 h4
                 h5
                 h6
                 head ; ~= block; useful for markdown
                 header
                 hgroup
                 hr
                 html ; ~= block; useful for markdown
                 iframe
                 ins
                 isindex
                 map
                 menu
                 noframes
                 noscript
                 object
                 ol
                 output
                 p
                 pre
                 progress
                 script
                 section
                 table
                 ul
                 video)))

(define inline-elements
  (apply seteq '(!HTML-COMMENT
                 a
                 abbr
                 address
                 applet
                 area
                 audio
                 b
                 bm
                 button
                 cite
                 code
                 del
                 details
                 dfn
                 command
                 datalist
                 em
                 font
                 i
                 iframe
                 img
                 input
                 ins
                 kbd
                 label
                 legend
                 link
                 map
                 mark
                 meter
                 nav
                 object
                 optgroup
                 option
                 q
                 script
                 select
                 small
                 source
                 span
                 strike
                 strong
                 sub
                 summary
                 sup
                 tbody
                 td
                 time
                 var)))

;; Pragmatic: Tolerate illegal "< " instead of "&lt; "
(define $lt-followed-by-space
  (try (pdo-one (~> (char #\<)) (lookAhead (char #\space)))))

(define $text
  (<?> (pdo (cs <- (many1 (<or> (noneOf "<& \n\r")
                                $lt-followed-by-space)))
            (return (list->string cs)))
       "normal char"))

(define $whitespace
  (>> (many1 (oneOf " \n\r"))
      (<or> (pdo (lookAhead $any-close-tag) (return ""))
            (return " "))))

(define $content
  (<?> (<or> $whitespace
             $entity
             $text
             $element)
       "content"))

(module+ test
  (with-parser (many $content)
    ["The lazy brown fox" '("The" " " "lazy" " " "brown" " " "fox")]
    ["&quot;" '(quot)]
    ["A &quot;" '("A" " " quot)]
    ["A&P" '("A" "&" "P")]))

(module+ test
  (with-parser $element
    ["<ul>\n <li>0</li>\n<li>1<li>2</ul>"
     '(ul () (li () "0") "" (li () "1") (li () "2"))]
    ["<div><p>0<p>1</div>"
     '(div () (p () "0") (p () "1"))]))

(define $document
  (pdo (many $junk)
       (open <- (open-tag 'html))
       (many $junk)
       (head <- (option #f (element 'head)))
       (many $junk)
       (body <- (option #f (element 'body)))
       (many $junk)
       (close-tag 'html)
       (many $junk)
       $eof
       (return (append open
                       (if head (list head) '())
                       (if body (list body) '())))))

(module+ test
  (with-parser $document
    ["<html></html>" '(html ())]
    ["<html><head>yo</head></html>" '(html () (head () "yo"))]
    ["<html><body>yo</body></html>" '(html () (body () "yo"))]
    ["<html><head>yo</head><body>yo</body></html>"
     '(html () (head () "yo") (body () "yo"))]))

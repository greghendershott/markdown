#lang at-exp racket

(require parsack)

(provide parse-markdown
         read-markdown)

(module+ test
  (require rackunit))

;; 1. Much of the grammer initially inspired by
;; https://github.com/jgm/markdown-peg/blob/master/Markdown.hs
;;
;; 2. To which I added extensions like fenced code blocks and
;; footnotes.
;;
;; 3. After difficulty parsing Markdown lists, got help looking at
;; http://hackage.haskell.org/package/open-pandoc-1.5.1.1/docs/src/Text-Pandoc-Readers-Markdown.html
;;
;; 4. Still to-do: Parsing raw HTML.


;; Add this one to parsack itself
(define (fail msg)
  (match-lambda
   [(and state (State inp pos))
    (Empty (Error (Msg pos inp (list (format "not ~a:" msg)))))]))

;; Add this one to parsack itself
(define (many1Till p end)
  (parser-compose (x <- p)
                  (xs <- (manyTill p end))
                  (return (cons x xs))))

(define (enclosed open close p)
  (try (parser-compose open
                       (notFollowedBy $space)
                       (xs <- (many1Till p close))
                       (return xs))))

(define (oneOfStrings . ss)
  (<?> (parser-compose (cs <- (choice (map (compose1 try string) ss)))
                       (return (list->string cs)))
       (string-append "one of: "
                      (string-join (map ~s ss) ", "))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Characters and tokens

(define space-chars " \t")
(define $space-char (<?> (oneOf space-chars) "space or tab"))
(define $sp (many $space-char))
(define $spnl (parser-seq $sp (option "" (parser-seq $newline $sp))))

(define special-chars "*_`&[]<!\\'\"-.")
(define $special-char (<?> (parser-one (~> (oneOf special-chars)))
                           "special char"))

(define $escaped-char (<?> (parser-one (char #\\) (~> $anyChar))
                           "escaped char"))

(define $normal-char (<?> (<or> $escaped-char
                                (noneOf (string-append space-chars
                                                       special-chars
                                                       "\n")))
                          "normal char"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Strings

(define $non-indent-space
  (<or> (parser-compose (try (string "   ")) (return 3))
        (parser-compose (try (string "  "))  (return 2))
        (parser-compose (try (string " "))   (return 1))
        (return 0)))
(define $indent (<or> (string "\t") (string "    ")))
(define $indented-line (parser-one $indent (~> $any-line)))
(define $optionally-indented-line (parser-one (optional $indent)
                                              (~> $any-line)))
(define $any-line (parser-compose
                   (xs <- (many (noneOf "\n")))
                   $newline
                   (return (list->string xs))))
(define $blank-line (try (parser-compose $sp $newline (return "\n"))))
(define $blockquote-line (parser-one $non-indent-space
                                     (char #\>)
                                     (optional (char #\space))
                                     (~> $any-line)))


(define (quoted c)
  (try (>>= (between (char c)
                     (char c)
                     (many (noneOf (make-string 1 c))))
            (compose1 return list->string))))
(define $single-quoted (quoted #\'))
(define $double-quoted (quoted #\"))
(define $quoted (<or> $single-quoted $double-quoted))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; general purpose

(require (rename-in racket [string rkt:string]))

(define (chars-in-balanced open close)
  (define (inner open close)
    (try (parser-one
          (char open)
          (~> (many (<or> (many1 (noneOf (rkt:string open close)))
                          (parser-compose
                           (xs <- (inner open close))
                           (return (append (list open) xs (list close)))))))
          (char close))))
  (parser-seq (inner open close)
              #:combine-with (compose1 list->string flatten)))

(module+ test
  (check-equal? (parse-result (chars-in-balanced #\< #\>) "<yo <yo <yo>>>")
                "yo <yo <yo>>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HTML

(define $html-comment
  (try
   (parser-compose
    (xs <- (between (string "<!--")
                    (string "-->")
                    (many (parser-one (notFollowedBy (string "-->"))
                                      (~> $anyChar)))))
    (return `(!HTML-COMMENT () ,(list->string xs))))))

(define $html-attribute
  (parser-compose
   (key <- (many1 (<or> $letter $digit)))
   $spnl
   (option "" (string "="))
   $spnl
   (val <- (<or> $quoted
                 (many1 (parser-one (noneOf space-chars)
                                    (~> $anyChar)))))
   $spnl
   (return (list (string->symbol (list->string key))
                 val))))

(define $html-element/self-close
  (try
   (parser-compose (char #\<)
                   $spnl
                   (sym <- (>>= (many1 (<or> $letter $digit))
                                (compose1 return string->symbol list->string)))
                   $spnl
                   (as <- (>>= (many $html-attribute)
                               (compose1 return append)))
                   $spnl
                   (char #\/)
                   (char #\>)
                   $spnl
                   (return `(,sym ,as)))))

(define (close tag) ;; make a parser for a specific closing tag
  (try
   (parser-seq (char #\<)
               $sp
               (char #\/)
               $spnl
               (string tag)
               $spnl
               (char #\>))))

(define $html-element/pair
  (try
   (parser-compose (char #\<)
                   $spnl
                   (tag <- (>>= (many1 (<or> $letter $digit))
                                (compose1 return list->string)))
                   $spnl
                   (as <- (>>= (many $html-attribute)
                               (compose1 return append)))
                   $spnl
                   (char #\>)
                   $spnl
                   (xs <- (cond [(string-ci=? tag "pre")
                                 (parser-seq
                                  (many (parser-one (notFollowedBy (close tag))
                                                    (~> $anyChar)))
                                  #:combine-with (compose1 list list->string))]
                                [else
                                 (many (parser-one (notFollowedBy (close tag))
                                                   (~> $inline)))]))
                   (close tag)
                   $spnl
                   (return `(,(string->symbol tag)
                             ,as
                             ,@xs)))))

(define $html-element (<or> $html-element/self-close
                            $html-element/pair))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inline
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define ($strong state)
  (_$strong state)) ;; defined after $inline

(define ($emph state)
  ($_emph state))   ;; defined after $inline and $strong

(define (ticks n)
  (parser-compose (string (make-string n #\`)) (notFollowedBy (char #\`))))
(define (between-ticks n)
  (try
   (between (ticks n)
            (ticks n)
            (parser-compose
             (xs <- (many1 (<or> (many1 (noneOf "`"))
                                 (parser-compose
                                  (notFollowedBy (ticks n))
                                  (xs <- (many1 (char #\`)))
                                  (return xs)))))
             (return `(code () ,(string-trim (list->string (append* xs)))))))))
(define codes (for/list ([n (in-range 10 0 -1)])
                (between-ticks n)))
(define $code (apply <or> codes))

(define $str (try (>>= (many1 $normal-char) (compose1 return list->string))))

(define $special (>>= $special-char (compose1 return (curry make-string 1))))

(define in-list-item? (make-parameter #f))
(define $end-line (try (parser-compose $newline
                                       (notFollowedBy $blank-line)
                                       (if (in-list-item?)
                                           (notFollowedBy $list-start)
                                           (return null))
                                       (return " "))))

(define $line-break (try (parser-compose (string " ") $sp $end-line
                                         (return `(br ())))))

(define $_spaces (>>= (many1 $space-char) (const (return " "))))

(define $whitespace
  (<or> $line-break
        $_spaces))

(define $char-entity (try (parser-compose
                           (char #\&)
                           (char #\#)
                           (<or> (char #\x)
                                 (char #\X))
                           (x <- (many1 $hexDigit))
                           (char #\;)
                           (return (integer->char
                                    (string->number (list->string x)
                                                    16))))))

(define $sym-entity (try (parser-compose
                          (char #\&)
                          (x <- (many1 (<or> $letter $digit)))
                          (char #\;)
                          (return (string->symbol (list->string x))))))

(define $entity (<or> $char-entity $sym-entity))

;;----------------------------------------------------------------------
;; smart punctuation

(define $smart-em-dash
  (<or>
   (try (parser-compose (char #\-) (char #\-) (char #\-) (return 'mdash)))
   (try (parser-compose (xs <- (many1 $alphaNum))
                        (char #\-) (char #\-)
                        (ys <- (many1 $alphaNum))
                        (return `(SPLICE ,(list->string xs)
                                         mdash
                                         ,(list->string ys)))))))
(define $smart-en-dash
  (try (parser-compose (char #\-) (char #\-) (return 'ndash))))

(define $smart-dashes (<or> $smart-em-dash $smart-en-dash))

(define $smart-apostrophe
  (parser-compose
   (char #\')
   (return 'rsquo))) ;; could use 'apos for HTML5?

(define quote-context (make-parameter #f))

(define (fail-in-quote-context x)
  (if (equal? (quote-context) x)
      (fail "already in quote")
      (return null)))

(define $single-quote-start
  (parser-seq
   (fail-in-quote-context 'single)
   (try (parser-seq
         (char #\')
         (notFollowedBy (oneOf ")!],.;:-? \t\n"))
         (notFollowedBy (try (>> (oneOfStrings "s" "t" "m" "ve" "ll" "re")
                                 (satisfy (lambda (c)
                                            (and (not (char-alphabetic? c))
                                                 (not (char-numeric? c))))))))))))

(define $single-quote-end
  (parser-seq (char #\')
              (notFollowedBy $alphaNum)))

(define $smart-quoted/single
  (try (parser-compose $single-quote-start
                       (xs <- (parameterize ([quote-context 'single])
                                (many1Till $inline $single-quote-end)))
                       (return `(SPLICE lsquo ,@xs rsquo)))))

(define $double-quote-start
  (parser-seq
   (fail-in-quote-context 'double)
   (try (parser-seq
         (char #\")
         (notFollowedBy (oneOf " \t\n"))))))

(define $double-quote-end
  (char #\"))

(define $smart-quoted/double
  (try (parser-compose $double-quote-start
                       (xs <- (parameterize ([quote-context 'double])
                                (manyTill $inline $double-quote-end)))
                       (return `(SPLICE ldquo ,@xs rdquo)))))

(define $smart-quoted (<or> $smart-quoted/single
                            $smart-quoted/double
                            ))

(define $smart-ellipses
  (<?> (parser-compose (oneOfStrings "..." " . . . " ". . ." " . . .")
                       (return 'hellip))
       "ellipsis"))

(define $smart-punctuation
  (<or> $smart-quoted
        $smart-apostrophe
        $smart-dashes
        $smart-ellipses))

;;----------------------------------------------------------------------

(define $footnote-label (try (parser-compose (char #\[)
                                             (char #\^)
                                             (xs <- (many (noneOf "]")))
                                             (char #\])
                                             (return (list->string xs)))))

(define $label (chars-in-balanced #\[ #\]))

(define $footnote-ref
  (try (parser-compose
        (label <- $footnote-label)
        (return (let* ([num (add-footnote-ref! (ref:back label))]
                       [anchor (~a (footnote-prefix) "-footnote-" num "-return")])
                  `(sup () (a ([href ,(ref:note label)]
                               [name ,anchor])
                              ,(~a num))))))))

(define $title (>>= (between (char #\")
                             (char #\")
                             (many (noneOf "\"")))
                    (compose1 return list->string)))

(define $source (>>= (many1 (noneOf "()> \n\t"))
                     (compose1 return list->string)))

(define $source+title (parser-compose
                       (char #\()
                       (src <- $source)
                       $sp
                       (tit <- (option "" (parser-one $sp (~> $title) $sp)))
                       (char #\))
                       (return (list src tit))))

(define $explicit-link (try (parser-compose
                             (label <- $label)
                             (src+tit <- $source+title)
                             (return (cons label src+tit)))))

(define $reference-link (try (parser-compose
                              (label <- $label)
                              $spnl
                              (href <- $label)
                              (let ([href (ref:link (match href ["" label] [x x]))])
                                (return (list label href ""))))))

(define $_link (<or> $explicit-link $reference-link))
(define $link (>>= $_link (match-lambda
                           [(list label src title)
                            (return `(a ([href ,src])
                                        ,@(parse-markdown* label)))])))
(define $image (try (parser-compose (char #\!)
                                    (x <- $_link)
                                    (return (match x
                                              [(list label src title)
                                               `(img ([src ,src]
                                                      [alt ,label]
                                                      [title ,title]))])))))

(define $autolink/url
  (try
   (parser-one
    (char #\<)
    (~> (parser-seq (many1 (noneOf ":"))
                    (char #\:) (char #\/) (char #\/)
                    (many1 (noneOf "\n>"))
                    #:combine-with
                    (lambda xs
                      (define s (list->string (flatten xs)))
                      `(a ([href ,s]) ,s))))
    (char #\>))))

(define $autolink/email
  (try
   (parser-one
    (char #\<)
    (~> (parser-seq (many1 (noneOf "@"))
                    (char #\@)
                    (many1 (noneOf "\n>"))
                    #:combine-with
                    (lambda xs
                      (define s (list->string (flatten xs)))
                      `(a ([href ,(string-append "mailto:" s)]) ,s))))
    (char #\>))))

(define $autolink (<or> $autolink/url $autolink/email))

(define $html/inline (<or> $html-comment $html-element))

;; Idea from pandoc: To avoid perf probs, parse 4+ * or _ as literal
;; instead of attempting to parse as emph or strong.
(define (4+ c)
  (define 4s (make-string 4 c))
  (try (parser-compose (string 4s)
                       (xs <- (many (char c)))
                       (return (string-append 4s (list->string xs))))))

(define $inline (<?> (<or> $str
                           $smart-punctuation
                           $whitespace
                           $end-line
                           $code
                           (<or> (4+ #\*) (4+ #\_))
                           $strong
                           $emph
                           $footnote-ref
                           $link
                           $image
                           $autolink
                           $html/inline
                           $entity
                           $special)
                     "inline"))

;; Must define after $inline
(define _$strong
  (parser-compose
   (xs <- (<or> (enclosed (string "**") (try (string "**")) $inline)
                (enclosed (string "__") (try (string "__")) $inline)))
   (return `(strong () ,@xs))))

;; Must define after $inline
(define $_emph
  (parser-compose
   (xs <- (<or> (enclosed (parser-seq (char #\*) (lookAhead $alphaNum))
                          (parser-seq (notFollowedBy $strong) (char #\*))
                          $inline)
                (enclosed (parser-seq (char #\_) (lookAhead $alphaNum))
                          (parser-seq
                           (parser-seq (notFollowedBy $strong) (char #\_))
                           (notFollowedBy $alphaNum))
                          $inline)))
   (return `(em () ,@xs))))

(module+ test
  ;; All 8 permutations
  (define s/e '((strong () "Bold " (em () "italic") " bold")))
  (check-equal? (parse-markdown "**Bold *italic* bold**") s/e)
  (check-equal? (parse-markdown "**Bold _italic_ bold**") s/e)
  (check-equal? (parse-markdown "__Bold _italic_ bold__") s/e)
  (check-equal? (parse-markdown "__Bold *italic* bold__") s/e)

  (define e/s '((em () "Italic " (strong () "bold") " italic")))
  (check-equal? (parse-markdown "*Italic **bold** italic*") e/s)
  (check-equal? (parse-markdown "*Italic __bold__ italic*") e/s)
  (check-equal? (parse-markdown "_Italic __bold__ italic_") e/s)
  (check-equal? (parse-markdown "_Italic **bold** italic_") e/s)

  (check-equal? (parse-markdown "no __YES__ no __YES__")
                '("no " (strong () "YES") " no " (strong () "YES")))
  (check-equal? (parse-markdown "no **YES** no **YES**")
                '("no " (strong () "YES") " no " (strong () "YES")))
  (check-equal? (parse-markdown "** no no **")
                '("** no no **"))
  (check-equal? (parse-markdown "no ____ no no")
                '("no ____ no no"))
  (check-equal? (parse-markdown "__Bold with `code` inside it.__")
                '((strong () "Bold with " (code () "code") " inside it."))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Block
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $para (try (parser-compose (xs <- (many1 $inline))
                                   $newline
                                   (many1 $blank-line)
                                   (return `(p () ,@xs)))))

(define $plain (try (parser-compose (xs <- (many1 $inline))
                                    (optional $blank-line)
                                    (return `(SPLICE ,@xs)))))

(define $blockquote (try (parser-compose
                          (xs <- (many1 $blockquote-line))
                          (ys <- (many (parser-one (notFollowedBy $blank-line)
                                                   (~> $any-line))))
                          (many $blank-line)
                          (return
                           (let* ([raw (string-append
                                        (string-join (append xs ys) "\n")
                                        "\n\n")]
                                  [xexprs (parse-markdown* raw)])
                             `(blockquote () ,@xexprs))))))
                           
(module+ test
  (check-equal?
   (parse-markdown @~a{> Foo
                       > Foo
                       >
                       > Foo
                       > Foo
                       })
   '((blockquote () (p () "Foo Foo") (p () "Foo Foo")))))

(define $verbatim/indent (try (parser-compose
                               (xs <- (many1 $indented-line))
                               (many1 $blank-line)
                               (return `(pre () ,(string-join xs "\n"))))))

(define $fence-line-open (parser-compose (string "```")
                                         (xs <- (many (noneOf "\n")))
                                         $newline
                                         (return (list->string xs))))
(define $fence-line-close (parser-seq (string "```") $newline))

(define $not-fence-line (parser-one (notFollowedBy $fence-line-close)
                                    (~> $any-line)))
(define $verbatim/fenced (try (parser-compose
                               (lang <- $fence-line-open)
                               (xs <- (many $not-fence-line))
                               $fence-line-close
                               (return
                                (let ([text (string-join xs "\n")])
                                  (match lang
                                    ["" `(pre () ,text)]
                                    [_  `(pre ([class ,(format "brush: ~a"
                                                               lang)])
                                              ,text)]))))))

(define $verbatim (<or> $verbatim/indent $verbatim/fenced))

(define (atx-hn n) (try (parser-compose
                         (string (make-string n #\#))
                         $sp
                         (xs <- (many1 (parser-one (notFollowedBy $newline)
                                                   (~> $inline))))
                         $newline
                         $spnl
                         (return (let ([sym (string->symbol (format "h~a" n))])
                                   `(,sym () ,@xs))))))
(define atx-hns (for/list ([n (in-range 6 0 -1)]) ;order: h6, h5 ... h1
                  (atx-hn n)))
(define $atx-heading (apply <or> atx-hns))

(define (setext sym c) ;; (or/c 'h1 'h2) char? -> xexpr?
  (try (parser-compose
        (xs <- (many1 (parser-one (notFollowedBy $end-line) (~> $inline))))
        $newline
        (string (make-string 3 c))
        (many (char c))
        $newline
        $spnl
        (return `(,sym () ,@xs)))))
(define setext-hns (list (setext 'h1 #\=) (setext 'h2 #\-)))
(define $setext-heading (apply <or> setext-hns))

(define $heading (<or> $atx-heading $setext-heading))

(define (hr c) (try (parser-compose
                     $non-indent-space
                     (char c) $sp (char c) $sp (char c) $sp
                     (many (parser-seq (char c) $sp))
                     $newline
                     (many1 $blank-line)
                     (return `(hr ())))))

(define $hr (<or> (hr #\*) (hr #\_) (hr #\-)))

(define $footnote-def
  (try (parser-compose $non-indent-space
                       (label <- $footnote-label)
                       (char #\:)
                       $spnl
                       (optional $blank-line)
                       (optional $indent)
                       (xs <- (sepBy $raw-lines
                                     (try (>> $blank-line $indent))))
                       (optional $blank-line)
                       (return
                        (let* ([num (get-ref (ref:back label))]
                               [back-href (~a "#" (footnote-prefix)
                                              "-footnote-" num "-return")]
                               [anchor (~a (footnote-prefix) "-footnote-"
                                           num "-definition")]
                               [s (~a num ": " (string-join xs "\n")
                                      "[↩](" back-href ")"
                                      "\n\n")]
                               [xexprs (parse-markdown* s)])
                          (add-ref! (ref:note label) (~a "#" anchor))
                          `(div ([id ,anchor]
                                 [class "footnote-definition"])
                                ,@xexprs))))))

(module+ test
  (let ()
    (define prefix "foo")
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
       (div ([id "foo-footnote-1-definition"]
             [class "footnote-definition"])
            (p () "1: The first paragraph of the definition.")
            (p () "Paragraph two of the definition.")
            (blockquote () (p () "A blockquote with multiple lines."))
            (pre () "a code block\n here")
            (p () "A final paragraph. "
               (a ([href "#foo-footnote-1-return"]) "↩")))
       (p () "Not part of defn.")))))

(define $raw-line (parser-compose (notFollowedBy $blank-line)
                                  (notFollowedBy $footnote-label)
                                  (xs <- (many1 (noneOf "\n")))
                                  (end <- (option "" (parser-compose
                                                      $newline
                                                      (optional $indent)
                                                      (return "\n"))))
                                  (return (~a (list->string xs) end))))

(define $raw-lines (>>= (many1 $raw-line) (compose1 return string-join)))

(define $reference (try (parser-compose
                         $non-indent-space
                         (label <- $label)
                         (char #\:)
                         $spnl
                         (src <- (parser-compose
                                  (xs <- (many1 (parser-one
                                                 (notFollowedBy $space-char)
                                                 (notFollowedBy $newline)
                                                 (~> $anyChar))))
                                  (return (list->string xs))))
                         $spnl
                         (title <- (option "" $title))
                         (many $blank-line)
                         (return (let () (add-ref! (ref:link label) src)
                                      "")))))

;; (module+ test
;;   (let ()
;;     (define-syntax-rule (chk s)
;;       (check-equal?
;;        (parse-markdown (~a "See [foo][].\n\n" s "\n\n"))
;;        '((p () "See " (a ((href "http://example.com/")) "foo") "."))))
;;     (chk "[foo]: http://example.com/  \"Optional Title Here\"")
;;     (chk "   [foo]:   http://example.com/     \"Optional Title Here\"")
;;     (chk "[foo]: http://example.com/  'Optional Title Here'")
;;     (chk "[foo]: http://example.com/  (Optional Title Here)")))

(define $html/block (<or> $html-comment $html-element))

;;----------------------------------------------------------------------
;; list blocks
;;
;; this modeled after pandoc

(define $bullet-list-start (try (parser-compose
                                 (optional $newline)
                                 (x <- $non-indent-space)
                                 (notFollowedBy $hr)
                                 (oneOf "+*-")
                                 (many1 $space-char)
                                 (return x))))

(define $ordered-list-start (try (parser-compose
                                  (optional $newline)
                                  (x <- $non-indent-space)
                                  (many1 $digit)
                                  (char #\.)
                                  (many1 $space-char)
                                  (return x))))

(define $list-start (<?> (<or> $bullet-list-start
                               $ordered-list-start)
                         "start of bullet list or ordered list"))

(define $list-line (try (parser-compose
                         (notFollowedBy $list-start)
                         (notFollowedBy $blank-line)
                         (notFollowedBy (parser-seq $indent
                                                    (many $space-char)
                                                    $list-start))
                         (xs <- (manyTill (<or> $html-comment $anyChar)
                                          $newline))
                         (return (string-append (list->string xs) "\n")))))

(define $raw-list-item (try (parser-compose
                             $list-start
                             (xs <- (many1 $list-line))
                             (_s <- (many $blank-line)) ;; "\n"
                             (return (string-join (append xs _s) "")))))

;; Continuation of a list item, indented and separated by $blank-line
;; or (in compact lists) endline.
;; Nested lists are parsed as continuations
(define $list-continuation (try (parser-compose
                                 (lookAhead $indent)
                                 (xs <- (many1 $list-continuation-line))
                                 (_s <- (many $blank-line))
                                 (return (append xs _s)))))

(define $list-continuation-line (try (parser-compose
                                      (notFollowedBy $blank-line)
                                      (notFollowedBy $list-start)
                                      (optional $indent)
                                      (xs <- (manyTill $anyChar $newline))
                                      (return (string-append (list->string xs)
                                                             "\n")))))

(define $list-item ;; -> xexpr?
  (try (parser-compose
        (s <- $raw-list-item)
        (ss <- (many $list-continuation))
        (return (let ([raw (string-join (cons s (append* ss)) "")])
                  `(li () ,@(parameterize ([in-list-item? #t])
                              (parse-markdown* raw))))))))

(define $ordered-list (try (parser-compose
                            (lookAhead $ordered-list-start)
                            (xs <- (many1 $list-item))
                            (return `(ol () ,@(maybe-tighten xs))))))

(define $bullet-list (try  (parser-compose
                            (lookAhead $bullet-list-start)
                            (xs <- (many1 $list-item))
                            (return `(ul () ,@(maybe-tighten xs))))))

;; If all but the last li aren't p's, remove the p from the last one
(define (maybe-tighten xs)
  (let loop ([all-tight? #t]
             [xs xs])
    (match xs
      ['() '()]
      [(cons (and x `(li () (p () ,els ...))) '()) ;; last
       (list (cond [all-tight? `(li () ,@els)]
                   [else x]))]
      [(cons (and x `(li () (p ,_ ...))) more) ;; loose
       (cons x (loop #f more))]
      [(cons x more) ;; right
       (cons x (loop (or all-tight? #t) more))])))

(module+ test
  (check-equal?
   (maybe-tighten `((li () "a") (li () "b") (li () (p () "c"))))
   `((li () "a") (li () "b") (li () "c")))
  (check-equal?
   (maybe-tighten `((li () (p () "a")) (li () "b") (li () (p () "c"))))
   `((li () (p () "a")) (li () "b") (li () "c"))))

(define $list (<or> $ordered-list $bullet-list))

(module+ test
  ;; Loose
  (check-equal? (parse-markdown @~a{- One.
                                      
                                    - Two.
                                    
                                    })
                '((ul () (li () (p () "One."))
                         (li () (p () "Two.")))))
  ;; Tight
  (check-equal? (parse-markdown @~a{- One.
                                    - Two.
                                    })
                '((ul () (li () "One.")
                         (li () "Two."))))
  ;; Indented < 4 spaces, loose
  (check-equal? (parse-markdown @~a{  - One.
                                      
                                      - Two.
                                     
                                    })
                '((ul () (li () (p () "One."))
                         (li () (p () "Two.")))))
  ;; Ordered
  (check-equal? (parse-markdown @~a{1. One.
                                    
                                    2. Two.
                                    
                                    })
                '((ol () (li () (p () "One."))
                         (li () (p () "Two."))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $block (<?> (<or> $blockquote
                          $verbatim
                          $footnote-def
                          $reference
                          $html/block
                          $heading
                          $list
                          $hr
                          $para
                          $plain)
                    "block"))

(define $markdown (parser-one (many $blank-line)
                              (~> (many $block))
                              (many $blank-line)
                              $eof))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; References (both reference links and footnotes)

(struct ref (id) #:transparent)         ;#:transparent so equal? works
(struct ref:link ref () #:transparent)  ;"  id is string?
(struct ref:note ref () #:transparent)  ;"  id is string?
(struct ref:back ref () #:transparent)  ;"  id is integer?

(define current-refs (make-parameter (make-hash))) ;ref? => ?

(define (resolve-refs xs) ;; (listof xexpr?) -> (listof xexpr?)
  ;; Walk the ~xexprs looking for 'a elements whose 'href attribute is
  ;; ref?, and replace with hash value. Same for 'img elements 'src
  ;; attributes that are ref:link?
  (define (uri u)
    (cond [(ref? u) (get-ref u)]
          [else u]))
  (define (do-xpr x)
    (match x
      [`(a ,(list-no-order `[href ,href] more ...) ,body ...)
       `(a ([href ,(uri href)] ,@more) ,@(map do-xpr body))]
      [`(img ,(list-no-order `[src ,src] more ...) ,body ...)
       `(img ([src ,(uri src)] ,@more) ,@(map do-xpr body))]
      [`(,tag ([,k ,v] ...) ,body ...)
       `(,tag ,(map list k v) ,@(map do-xpr body))]
      [`(,tag ,body ...)
       `(,tag ,@(map do-xpr body))]
      [_ x]))
  (for/list ([x xs])
    (do-xpr x)))

(define (add-ref! ref uri) ;; ref? string? -> any
  (hash-set! (current-refs) ref uri))

(define (get-ref ref) ;; ref? -> string?
  (or (dict-ref (current-refs) ref #f)
      (begin (eprintf "Unresolved reference: ~v\n" ref) "")))

(module+ test
  (check-equal? (parameterize ([current-refs (make-hash)])
                  (add-ref! (ref:link "foo") "bar")
                  (resolve-refs `((a ([href ,(ref:link "foo")]) "foo"))))
                '((a ((href "bar")) "foo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Footnotes

(define footnote-number (make-parameter 0))
(define footnote-prefix (make-parameter (gensym)))

(define (add1-footnote-number) (curry add1-param footnote-number))

(define (add-footnote-ref! ref) ;; ref? -> integer?  [idempotent]
  (unless (hash-has-key? (current-refs) ref)
    (hash-set! (current-refs) ref (add1-footnote-number)))
  (get-ref ref))

(define (add1-param p) ;; parameter/c -> integer?
  (define v (add1 (p)))
  (p v)
  v)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Post processing on the xexprs

;; normalize : xexpr? -> xexpr?
;;
;; Do some recursive normalizations on the xexpr:
;; 1. Append consecutive string? elements in the xexpression.
;; 2. Delete any "" elements left after 1.
;; 3. Delete any trailing spaces in the last element.
;; 4. Splice any (@SPLICE) elements like unquote-splicing.
(define (normalize x)
  (match x
    [`(,tag ,as ,es ...)
     `(,tag ,as ,@(let loop ([es (splice es)]) ;; 4
                   (match es
                     [(list (? string? this) (? string? next) more ...) ;; 1
                      (loop (cons (string-append this next) more))]
                     [(cons "" more)    ;; 2
                      (loop more)]
                     [(cons (pregexp "^(.*?)\\s*$" (list _ this)) '()) ;; 3
                      (match this
                        ["" '()]
                        [_ (cons this '())])]
                     [(cons this more)
                      (cons (normalize this) (loop more))]
                     ['() '()])))]
    [x x]))

(module+ test
  (check-equal? (normalize `(p () "a" "b" "c" "d" "e" (p () "1" "2" "3 ")))
                '(p () "abcde" (p () "123"))))

;; normalize-xexprs : (listof xexpr?) -> (listof xexpr?)
;;
;; Like normalize but for a (listof xexpr?) not just one.
(define (normalize-xexprs xs)
  (match (normalize `(_ () ,@xs))
    [`(_ () ,xs ...) xs]))

;; splice : (listof xexpr?) -> (listof xexpr?)
;;
;; Do the equivalent of ,@ a.k.a. unquote-splicing recursively on all
;; `(@SPLICE es ...)` elements, such that the `es` get lifted/spliced.
(define (splice xs)
  (let loop ([xs xs])
    (match xs
      [`((SPLICE ,es ...) ,more ...)
       (loop (append es more))]
      [(cons this more)
       (cons this (loop more))]
      [(list) (list)])))

(module+ test
  (check-equal? (splice `((p () "A")
                          (SPLICE "a" "b")
                          (p () "B")))
                `((p () "A") "a" "b" (p () "B")))
  (check-equal? (normalize `(p () "a" "b" (SPLICE "c" "d") "e" "f"))
                `(p () "abcdef"))
  (check-equal? (normalize `(p () "a" (SPLICE "b" (SPLICE "c") "d") "e"))
                `(p () "abcde")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is to process an entire Markdown docuemnt.
;; Sets parameters like footnote nubmer to 0.
;; Appends a "\n" to `input` to simplify whole-docuemnt parsing.
(require rackjure/threading)
(define (parse-markdown s [footnote-prefix-symbol (gensym)])
  (parameterize ([current-refs (make-hash)]
                 [footnote-number 0]
                 [footnote-prefix footnote-prefix-symbol])
    (~>> (parse-markdown* (string-append s "\n"))
         resolve-refs)))

;; Use this internally to recursively parse fragments of Markdown
;; within the document. Does NOT set parameters. Does not append "\n"
;; to the string; up to caller to do so if required.
(define (parse-markdown* s)
  (~>> (parse-result $markdown s)
       normalize-xexprs))

(define (parse-result p s)
  (match (parse p s)
    [(Consumed! (Ok parsed _ _)) parsed]
    [x (error 'parse-result (~v x))]))

(define (read-markdown [footnote-prefix-symbol (gensym)])
  (parse-markdown (port->string (current-input-port)) footnote-prefix-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; randomized testing

(define (random-char)
  (let loop ()
    (define c (integer->char (random 127)))
    (cond [(or (char-alphabetic? c)
               (char-numeric? c)
               (memq c '(#\< #\> #\[ #\] #\( #\) #\_ #\newline)))
           c]
          [(loop)])))

(define (random-word)
  (list->string (for/list ([i (in-range (add1 (random 10)))])
                  (random-char))))

(define (random-line)
  (string-join (for/list ([i (in-range (+ 5 (random 15)))])
                 (random-word))
               " "))

(define (random-doc num-lines)
  (string-join (for/list ([n (in-range num-lines)])
                 (random-line))
               "\n\n"))

;; (module+ test
;;   ;; No input should ever cause a parse error or non-termination.
;;   ;; i.e. Random text is itself a valid Markdown format file.
;;   (for ([i 5])
;;     (check-not-exn (lambda () (parse-markdown (random-doc 50))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example

(define input #<<EOF
Heading 1
=========
Heading 2
---------
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6

Some **bold** text and _emph_ text and `inline code`.

    verbatim1
    verbatim2

```
Fenced with no lang.
```

Foo:

```racket
(define (x2 x)
  (* x 2))
```

Line 2. [Racket](http://www.racket-lang.org).

> I am blockquote 1.
> I am blockquote 2.

An ordered, tight list:

1. One
2. Two
3. Three

An ordered, loose list:

1. One

2. Two

3. Three

An unordered, tight list:

- One
- Two
- Three

An unordered, loose list:

- One

- Two

- Three

Nested lists:

1. One

    - 1A

    - 1B

2. Two

    - 2A

    - 2B

[ref1][].

<br /> 1 < 2 [1]. Amazing!

Some `<hr>`s:

---

- - -

--------


***

___


A [ref1][] and a [ref2][ref2].

[ref1]: http://www.google.com
[ref2]: http://www.google.com "foo"

Here's a table:

<table border="1">
<tr>
<td>Row 1 _Col_ 1</td>
<td>Row 1 Col 2</td>
</tr>
<tr>
<td>Row 2 Col 1</td>
<td>Row 2 Col 2</td>
</tr>
</table>

Here is a footnote use[^1]. And another[^2].

[^1]: The first paragraph of the definition.
    
    Paragraph two of the definition.
    
    > A blockquote with
    > multiple lines.
    
        a code block
    
    A final paragraph.

[^2]: Another footnote defn.

The end.

Some hard line breaks...  
...with two spaces...  
...at end of each one.

Here's an appostrophe. 'Single quotes'. "Double quotes". "A really _great_ quote."  But don't convert <!-- more -->.

<!-- more -->

EOF
)

(require racket/runtime-path)
(define-runtime-path test.md "test/test.md")
;; (pretty-print (parse-markdown (file->string test.md)))

;; (pretty-print (parse-markdown input 'foo))

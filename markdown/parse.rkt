#lang at-exp racket

(require (rename-in parsack
                    [parser-compose pdo]  ;; More concise, less indent
                    [parser-one pdo-one]  ;; "
                    [parser-seq pdo-seq]) ;; "
         xml/xexpr
         rackjure/threading
         "xexpr.rkt"
         "xexpr2text.rkt")

(provide
 (contract-out
  [read-markdown (() (symbol?) . ->* . xexpr-element-list?)]
  [parse-markdown ((string?) (symbol?) . ->* . xexpr-element-list?)]))

(module+ test
  (require rackunit))

(define (xexpr-element-list? xs)
  (xexpr? `(dummy () ,@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is to process an entire Markdown document.
;; Sets parameters like footnote number to 0.
;; Appends a "\n" to `input` to simplify whole-document parsing.
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

;; For backward compatibility
(define (read-markdown [footnote-prefix-symbol (gensym)])
  (parse-markdown (port->string (current-input-port)) footnote-prefix-symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General purpose combinators
;;
;; Some could/should be moved to Parsack

(require (rename-in racket [string rkt:string])) ;; not Parsack's `string`
(define (chars-in-balanced open close) ;; char? char? -> parser?
  (define (inner open close)
    (try (pdo-one
          (char open)
          (~> (many (<or> (many1 (noneOf (rkt:string open close)))
                          (pdo (xs <- (inner open close))
                               (return (append (list open)
                                               xs
                                               (list close)))))))
          (char close))))
  (pdo-seq (inner open close)
           #:combine-with (compose1 list->string flatten)))

(module+ test
  (check-equal? (parse-result (chars-in-balanced #\< #\>) "<yo <yo <yo>>>")
                "yo <yo <yo>>"))

(define (enclosed open close p) ;; parser? parser? parser? -> (listof any/c)
  (try (pdo open
            (notFollowedBy $space)
            (xs <- (many1Till p close))
            (return xs))))

;; Add this one to parsack itself?
(define (fail msg)
  (match-lambda
   [(and state (State inp pos))
    (Empty (Error (Msg pos inp (list (format "not ~a:" msg)))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Characters and tokens

(define space-chars " \t")
(define $space-char (<?> (oneOf space-chars) "space or tab"))
(define $sp (many $space-char))
(define $spnl (pdo-one $sp (optional (pdo-seq $newline $sp))
                       (~> (return null))))

(define special-chars "*_`&[]<!\\'\"-.")
(define $special-char (<?> (pdo-one (~> (oneOf special-chars)))
                           "special char"))

(define $escaped-char (<?> (pdo-one (char #\\) (~> $anyChar))
                           "escaped char"))

(define $normal-char (<?> (<or> $escaped-char
                                (noneOf (~a space-chars special-chars "\n")))
                          "normal char"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Strings

(define $non-indent-space (<?> (pdo (oneOfStrings "   " "  " " " "")
                                    (return null))
                               "non-indent space"))
(define $indent (<or> (string "\t") (string "    ")))
(define $indented-line (pdo-one $indent (~> $any-line)))
(define $optionally-indented-line (pdo-one (optional $indent)
                                           (~> $any-line)))
(define $any-line (pdo (xs <- (many (noneOf "\n")))
                       $newline
                       (return (list->string xs))))
(define $blank-line (try (pdo $sp $newline (return "\n"))))
(define $blockquote-line (pdo-one $non-indent-space
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
;;
;; HTML

(define $html-comment
  (try (pdo (xs <- (between (string "<!--")
                            (string "-->")
                            (many (pdo-one (notFollowedBy (string "-->"))
                                           (~> $anyChar)))))
            (many $blank-line)
            (return `(!HTML-COMMENT () ,(list->string xs))))))

(define $html-attribute
  (try (pdo (key <- (many1 (<or> $letter $digit)))
            $spnl
            (optional (string "="))
            $spnl
            ;; HTML attribute value can be quoted, unquoted, or even
            ;; missing (in which last case treat it as "true").
            (val <- (option "true"
                            (<or> $quoted
                                  (>>= (many1 (noneOf (~a space-chars ">\n")))
                                       (compose return list->string)))))
            $spnl
            (return (list (string->symbol (list->string key))
                          val)))))

(define $html-tag+attributes
  ;; -> (list symbol? (listof (list/c symbol? string?)))
  (try (pdo $spnl
            (name <- (>>= (many1 (<or> $letter $digit))
                          (compose1 return string->symbol
                                    string-downcase list->string)))
            $spnl
            (attributes <- (>>= (many $html-attribute)
                                (compose1 return append)))
            $spnl
            (return (list name attributes)))))

(define (html-element/self-close block?)
  ;; -> (list symbol? (listof (list/c symbol? string?)))
  (try (pdo (char #\<)
            (name+attributes <- $html-tag+attributes)
            (char #\/)
            $spnl
            (char #\>)
            (cond [block? (many $blank-line)]
                  [else (return null)])
            (return name+attributes))))

(define $any-open-tag
  ;; -> (list symbol? (listof (list/c symbol? string?)))
  (try (pdo (char #\<)
            (name+attributes <- $html-tag+attributes)
            $spnl
            (char #\>)
            (return name+attributes))))

(define (open-tag tag)
  ;; specific open tag
  ;; (or/c string? symbol?) -> (list symbol? (listof (list/c symbol? string?)))
  (try (pdo (char #\<)
            $spnl
            (lookAhead (pdo-seq (stringAnyCase (~a tag)) $spnl))
            (name+attributes <- $html-tag+attributes)
            $spnl
            (char #\>)
            (return name+attributes))))

(define (close-tag tag)
  ;; specific close tag
  ;; (or/c string? symbol?) -> (list symbol? (listof (list/c symbol? string?)))
  (try (pdo (char #\<)
            $sp
            (char #\/)
            $spnl
            (stringAnyCase (~a tag))
            $spnl
            (char #\>)
            (return null))))

(define (balanced open close p #:combine-with [f list])
  (define (inner open close)
    (try (pdo-seq open
                  (many (<or> (many1 (pdo-one (notFollowedBy open)
                                              (notFollowedBy close)
                                              (~> p)))
                              (pdo-seq (inner open close))))
                  close
                  #:combine-with f)))
  (pdo-one (~> (inner open close))))

;; Try to parse a matching pair of open/close tags like <p> </p>, else
;; a sole open tag like <img> which we treat like a self-closing tag
;; <img /> i.e. void element.
(define (html-element block?)
  (try (pdo (name+attributes <- (lookAhead $any-open-tag))
            (xs <- (<or> (balanced (open-tag (car name+attributes))
                                   (close-tag (car name+attributes))
                                   $inline
                                   #:combine-with (lambda (open els _)
                                                    (append* open els)))
                         (open-tag (car name+attributes))))
            (cond [block? (many $blank-line)]
                  [else (return null)])
            (return xs))))
                        
(define (html-pre block?)
  (try (pdo (n+a <- (open-tag "pre"))
            (xs <- (many1 (pdo-one (notFollowedBy (close-tag "pre"))
                                   (~> $anyChar))))
            (close-tag "pre")
            (cond [block? (many $blank-line)]
                  [else (return null)])
            (return (append n+a (list (list->string xs)))))))

(define $html/block (<or> $html-comment
                          (html-pre #t)
                          (html-element/self-close #t)
                          (html-element #t)))

(define $html/inline (<or> $html-comment
                           (html-pre #t)
                           (html-element/self-close #f)
                           (html-element #f)))

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
  (pdo (string (make-string n #\`)) (notFollowedBy (char #\`))))

(define (between-ticks n)
  (try (between (ticks n)
                (ticks n)
                (pdo (xs <- (many1 (<or> (many1 (noneOf "`"))
                                         (pdo (notFollowedBy (ticks n))
                                              (xs <- (many1 (char #\`)))
                                              (return xs)))))
                     (return (string-trim (list->string (append* xs))))))))

(define codes (for/list ([n (in-range 10 0 -1)])
                (between-ticks n)))

(define $code
  (try (pdo (str <- (apply <or> codes))
            (lang <- (option #f $label)) ;; my custom extension
            (return (match lang
                      [#f `(code () ,str)]
                      [x  `(code ([class ,(~a "brush: " x)]) ,str)])))))

(define $str
  (try (>>= (many1 $normal-char)
            (compose1 return list->string))))

(define $special
  (>>= $special-char
       (compose1 return (curry make-string 1))))

(define in-list-item? (make-parameter #f))
(define $end-line
  (try (pdo $newline
            (notFollowedBy $blank-line)
            (if (in-list-item?)
                (notFollowedBy $list-start)
                (return null))
            (return " "))))

(define $line-break
  (try (pdo (string " ")
            $sp $end-line
            (return `(br ())))))

(define $_spaces
  (>>= (many1 $space-char)
       (const (return " "))))

(define $whitespace
  (<or> $line-break
        $_spaces))

(define $char-entity
  (try (pdo (char #\&)
            (char #\#)
            (<or> (char #\x)
                  (char #\X))
            (x <- (many1 $hexDigit))
            (char #\;)
            (return (string->number (list->string x) 16)))))

(define $sym-entity
  (try (pdo (char #\&)
            (x <- (many1 (<or> $letter $digit)))
            (char #\;)
            (return (string->symbol (list->string x))))))

(define $entity (<or> $char-entity $sym-entity))

;;----------------------------------------------------------------------
;; smart punctuation

(define $smart-em-dash
  (<or> (try (>> (string "---")
                 (return 'mdash)))
        (try (pdo (xs <- (many1 $alphaNum))
                  (string "--")
                  (ys <- (many1 $alphaNum))
                  (return `(SPLICE ,(list->string xs)
                                   mdash
                                   ,(list->string ys)))))))
(define $smart-en-dash
  (try (>> (string "--") (return 'ndash))))

(define $smart-dashes (<or> $smart-em-dash $smart-en-dash))

(define $smart-apostrophe
  (>> (char #\') (return 'rsquo))) ;; could use 'apos for HTML5?

(define quote-context (make-parameter #f))

(define (fail-in-quote-context x)
  (if (equal? (quote-context) x)
      (fail "already in quote")
      (return null)))

(define $single-quote-start
  (pdo-seq
   (fail-in-quote-context 'single)
   (try (pdo-seq
         (char #\')
         (notFollowedBy (oneOf ")!],.;:-? \t\n"))
         (notFollowedBy (try (>> (oneOfStrings "s" "t" "m" "ve" "ll" "re")
                                 (satisfy (lambda (c)
                                            (and (not (char-alphabetic? c))
                                                 (not (char-numeric? c))))))))))))

(define $single-quote-end
  (>> (char #\') (notFollowedBy $alphaNum)))

(define $smart-quoted/single
  (try (pdo $single-quote-start
            (xs <- (parameterize ([quote-context 'single])
                     (many1Till $inline $single-quote-end)))
            (return `(SPLICE lsquo ,@xs rsquo)))))

(define $double-quote-start
  (pdo-seq (fail-in-quote-context 'double)
           (try (pdo-seq (char #\")
                         (notFollowedBy (oneOf " \t\n"))))))

(define $double-quote-end
  (char #\"))

(define $smart-quoted/double
  (try (pdo $double-quote-start
            (xs <- (parameterize ([quote-context 'double])
                     (manyTill $inline $double-quote-end)))
            (return `(SPLICE ldquo ,@xs rdquo)))))

(define $smart-quoted (<or> $smart-quoted/single
                            $smart-quoted/double))

(define $smart-ellipses
  (<?> (pdo (oneOfStrings "..." " . . . " ". . ." " . . .")
            (return 'hellip))
       "ellipsis"))

(define $smart-punctuation
  (<or> $smart-quoted
        $smart-apostrophe
        $smart-dashes
        $smart-ellipses))

;;----------------------------------------------------------------------

(define $footnote-label
  (try (pdo (char #\[)
            (char #\^)
            (xs <- (many (noneOf "]")))
            (char #\])
            (return (list->string xs)))))

(define $label (chars-in-balanced #\[ #\]))

(define $footnote-ref
  (try (pdo (label <- $footnote-label)
            (return
             (let* ([num (add-footnote-ref! (ref:back label))]
                    [anchor (~a (footnote-prefix) "-footnote-" num "-return")])
               `(sup () (a ([href ,(ref:note label)]
                            [name ,anchor])
                           ,(~a num))))))))

(define (link-title open [close open])
  (try (>>= (between (char open)
                     (char close)
                     (many (noneOf (make-string 1 close))))
            (compose1 return list->string))))

(define $link-title (<or> (link-title #\")
                          (link-title #\')
                          (link-title #\( #\))))

(define $source
  (>>= (many1 (noneOf "()> \n\t"))
       (compose1 return list->string)))

(define $source+title
  (pdo (char #\()
       (src <- $source)
       $sp
       (tit <- (option "" (pdo-one $sp (~> $link-title) $sp)))
       (char #\))
       (return (list src tit))))

(define $explicit-link
  (try (pdo (label <- $label)
            (src+tit <- $source+title)
            (return (cons label src+tit)))))

(define $reference-link
  (try (pdo (label <- $label)
            $spnl
            (ref <- $label)
            (let* ([id (match ref ["" label] [x x])]
                   [id (xexpr->slug id)]) ;'slug" ref link label xexpr
              (return (list label (ref:link id) ""))))))

(define $_link (<or> $explicit-link $reference-link))
(define $link
  (>>= $_link
       (match-lambda
        [(list label src title)
         (define xs (parse-markdown* label))
         (return (match title
                   ["" `(a ([href ,src])           ,@xs)]
                   [t  `(a ([href ,src][title ,t]) ,@xs)]))])))

(define $image
  (try (pdo (char #\!)
            (x <- $_link)
            (return
             (match x
               [(list label src title)
                (match title
                  ["" `(img ([src ,src][alt ,label]))]
                  [t  `(img ([src ,src][alt ,label][title ,t]))])])))))

(define $autolink/url
  (try (pdo-one (char #\<)
                (~> (pdo-seq (many1 (noneOf ":"))
                             (char #\:) (char #\/) (char #\/)
                             (many1 (noneOf "\n \"'<>"))
                             #:combine-with
                             (lambda xs
                               (define s (list->string (flatten xs)))
                               `(a ([href ,s]) ,s))))
                (char #\>))))

(define $autolink/email
  (try (pdo-one (char #\<)
                (~> (pdo-seq (many1 (noneOf "@"))
                             (char #\@)
                             (many1 (noneOf "\n>"))
                             #:combine-with
                             (lambda xs
                               (define s (list->string (flatten xs)))
                               `(a ([href ,(string-append "mailto:" s)]) ,s))))
                (char #\>))))

(define $autolink (<or> $autolink/url $autolink/email))

;; Idea from pandoc: To avoid perf probs, parse 4+ * or _ as literal
;; instead of attempting to parse as emph or strong.
(define (4+ c)
  (define 4s (make-string 4 c))
  (try (pdo (string 4s)
            (xs <- (many (char c)))
            (return (string-append 4s (list->string xs))))))

(define $inline
  (<?> (<or> $str
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
             $autolink ;; before html: faster
             $html/inline
             $entity
             $special)
       "inline"))

;; Must define after $inline
(define _$strong
  (pdo (xs <- (<or> (enclosed (string "**") (try (string "**")) $inline)
                    (enclosed (string "__") (try (string "__")) $inline)))
       (return `(strong () ,@xs))))

;; Must define after $inline
(define $_emph
  (pdo (xs <- (<or> (enclosed (pdo-seq (char #\*) (lookAhead $alphaNum))
                              (pdo-seq (notFollowedBy $strong) (char #\*))
                              $inline)
                    (enclosed (pdo-seq (char #\_) (lookAhead $alphaNum))
                              (pdo-seq (pdo-seq (notFollowedBy $strong)
                                                (char #\_))
                                       (notFollowedBy $alphaNum))
                              $inline)))
       (return `(em () ,@xs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Block
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $para
  (try (pdo (xs <- (many1 $inline))
            $newline
            (many1 $blank-line)
            (return `(p () ,@xs)))))

(define $plain
  (try (pdo (xs <- (many1 $inline))
            (optional $blank-line)
            (return `(SPLICE ,@xs)))))

(define $blockquote
  (try (pdo (xs <- (many1 $blockquote-line))
            (ys <- (many (pdo-one (notFollowedBy $blank-line)
                                  (~> $any-line))))
            (many $blank-line)
            (return (let* ([raw (string-append (string-join (append xs ys) "\n")
                                               "\n\n")]
                           [xexprs (parse-markdown* raw)])
                      `(blockquote () ,@xexprs))))))
                           
(define $verbatim/indent
  (try (pdo (xs <- (many1 $indented-line))
            (many1 $blank-line)
            (return `(pre () ,(string-join xs "\n"))))))

(define $fence-line-open
  (pdo (string "```")
       (xs <- (many (noneOf "\n")))
       $newline
       (return (list->string xs))))

(define $fence-line-close
  (pdo-seq (string "```") $newline))

(define $not-fence-line
  (pdo-one (notFollowedBy $fence-line-close)
           (~> $any-line)))

(define $verbatim/fenced
  (try (pdo (lang <- $fence-line-open)
            (xs <- (many $not-fence-line))
            $fence-line-close
            (return (let ([text (string-join xs "\n")])
                      (match lang
                        ["" `(pre () ,text)]
                        [_  `(pre ([class ,(format "brush: ~a" lang)])
                                  ,text)]))))))

(define $verbatim
  (<or> $verbatim/indent $verbatim/fenced))

(define (atx-hn n) ;; integer? -> xexpr?
  (try (pdo (string (make-string n #\#))
            $sp
            (xs <- (many1 (pdo-one (notFollowedBy $newline)
                                   (~> $inline))))
            $newline
            $spnl
            (return (let ([sym (string->symbol (format "h~a" n))]
                          [id (xexprs->slug xs)])
                      `(,sym ([id ,id]) ,@xs))))))

(define atx-hns (for/list ([n (in-range 6 0 -1)]) ;order: h6, h5 ... h1
                  (atx-hn n)))

(define $atx-heading (apply <or> atx-hns))

(define (setext sym c) ;; (or/c 'h1 'h2) char? -> xexpr?
  (try (pdo (xs <- (many1 (pdo-one (notFollowedBy $end-line) (~> $inline))))
            $newline
            (string (make-string 3 c))
            (many (char c))
            $newline
            $spnl
            (return (let ([id (xexprs->slug xs)])
                      `(,sym ([id ,id]) ,@xs))))))

(define setext-hns (list (setext 'h1 #\=) (setext 'h2 #\-)))

(define $setext-heading (apply <or> setext-hns))

(define $heading (<or> $atx-heading $setext-heading))

(define (hr c)
  (try (pdo $non-indent-space
            (char c) $sp (char c) $sp (char c) $sp
            (many (pdo-seq (char c) $sp))
            $newline
            (many1 $blank-line)
            (return `(hr ())))))

(define $hr (<or> (hr #\*) (hr #\_) (hr #\-)))

(define $footnote-def
  (try (pdo $non-indent-space
            (label <- $footnote-label)
            (char #\:)
            $spnl
            (optional $blank-line)
            (optional $indent)
            (xs <- (sepBy $raw-lines
                          (try (>> $blank-line $indent))))
            (optional $blank-line)
            (return (let* ([num (get-ref (ref:back label))]
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

(define $raw-line
  (pdo (notFollowedBy $blank-line)
       (notFollowedBy $footnote-label)
       (xs <- (many1 (noneOf "\n")))
       (end <- (option "" (pdo $newline
                               (optional $indent)
                               (return "\n"))))
       (return (~a (list->string xs) end))))

(define $raw-lines
  (>>= (many1 $raw-line)
       (compose1 return string-join)))

(define $reference
  (try (pdo $non-indent-space
            (label <- $label)
            (char #\:)
            $spnl
            (src <- (pdo (xs <- (many1 (pdo-one (notFollowedBy $space-char)
                                                (notFollowedBy $newline)
                                                (~> $anyChar))))
                         (return (list->string xs))))
            $spnl
            (title <- (option "" $link-title))
            (many $blank-line)
            (return (let ()
                      ;; The label is an xexpr so "slug" it
                      (add-ref! (ref:link (xexprs->slug label))
                                (cons src title))
                      "")))))

;;----------------------------------------------------------------------
;; list blocks
;;
;; this modeled after pandoc

(define $bullet-list-start
  (try (pdo (optional $newline)
            $non-indent-space
            (notFollowedBy $hr)
            (oneOf "+*-")
            (many1 $space-char)
            (return null))))

(define $ordered-list-start
  (try (pdo (optional $newline)
            $non-indent-space
            (many1 $digit)
            (char #\.)
            (many1 $space-char)
            (return null))))

(define $list-start
  (<?> (<or> $bullet-list-start
             $ordered-list-start)
       "start of bullet list or ordered list"))

(define $list-line
  (try (pdo (notFollowedBy $list-start)
            (notFollowedBy $blank-line)
            (notFollowedBy (pdo-seq $indent
                                    (many $space-char)
                                    $list-start))
            (xs <- (manyTill (<or> $html-comment $anyChar)
                             $newline))
            (return (string-append (list->string xs) "\n")))))

(define $raw-list-item
  (try (pdo $list-start
            (xs <- (many1 $list-line))
            (_s <- (many $blank-line)) ;; "\n"
            (return (string-join (append xs _s) "")))))

;; Continuation of a list item, indented and separated by $blank-line
;; or (in compact lists) endline.
;; Nested lists are parsed as continuations
(define $list-continuation
  (try (pdo (lookAhead $indent)
            (xs <- (many1 $list-continuation-line))
            (_s <- (many $blank-line))
            (return (append xs _s)))))

(define $list-continuation-line
  (try (pdo (notFollowedBy $blank-line)
            (notFollowedBy $list-start)
            (optional $indent)
            (xs <- (manyTill $anyChar $newline))
            (return (string-append (list->string xs) "\n")))))

(define $list-item ;; -> xexpr?
  (try (pdo (s <- $raw-list-item)
            (ss <- (many $list-continuation))
            (return (let ([raw (string-join (cons s (append* ss)) "")])
                      `(li () ,@(parameterize ([in-list-item? #t])
                                  (parse-markdown* raw))))))))

(define $ordered-list
  (try (pdo (lookAhead $ordered-list-start)
            (xs <- (many1 $list-item))
            (return `(ol () ,@(maybe-tighten xs))))))

(define $bullet-list
  (try (pdo (lookAhead $bullet-list-start)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define $block
  (<?> (<or> $blockquote
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

(define $markdown
  (pdo-one (many $blank-line)
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
  ;; Walk the xexprs looking for 'a elements whose 'href attribute is
  ;; ref?, and replace with hash value. Same for 'img elements 'src
  ;; attributes that are ref:link?
  (define (uri u)
    (cond [(ref? u) (match (get-ref u)
                      [(cons src title) src]
                      [src src])]
          [else u]))
  (define (title u)
    (cond [(ref? u) (match (get-ref u)
                      [(cons src title) title]
                      [_ ""])]
          [else ""]))
  (define (do-xpr x)
    (match x
      [`(a ,(list-no-order `[href ,href] more ...) ,body ...)
       (match (title href)
         ["" `(a ([href ,(uri href)]           ,@more) ,@(map do-xpr body))]
         [t  `(a ([href ,(uri href)][title ,t] ,@more) ,@(map do-xpr body))])]
      [`(img ,(list-no-order `[src ,src] more ...) ,body ...)
       (match (title src)
         ["" `(img ([src ,(uri src)]           ,@more) ,@(map do-xpr body))]
         [t  `(img ([src ,(uri src)][title ,t] ,@more) ,@(map do-xpr body))])]
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

(define (add-footnote-ref! ref) ;; ref? -> integer?  [idempotent]
  (unless (hash-has-key? (current-refs) ref)
    (hash-set! (current-refs) ref (add1-footnote-number)))
  (get-ref ref))

(define (add1-footnote-number)
  (curry add1-param footnote-number))

(define (add1-param p) ;; parameter/c -> integer?
  (define v (add1 (p)))
  (p v)
  v)

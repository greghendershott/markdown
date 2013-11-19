#lang at-exp racket

(require (only-in parsack
                  [parser-compose pdo] ;; More concise, less indent
                  [parser-one pdo-one] ;; "
                  [parser-seq pdo-seq] ;; "
                  >>= >>
                  try <or> <?>
                  satisfy char string stringAnyCase
                  many manyTill many1 many1Till
                  sepBy
                  oneOf noneOf oneOfStrings
                  option optional
                  return
                  between
                  lookAhead
                  notFollowedBy
                  $space $newline $anyChar $letter $digit $hexDigit
                  $alphaNum $eof
                  State State? Consumed Consumed! Empty Ok Error Msg
                  parse parse-result parsack-error)
         xml/xexpr
         rackjure/threading
         "xexpr.rkt"
         "xexpr2text.rkt"
         "void-element.rkt")

(provide
 (contract-out
  [read-markdown (() (symbol?) . ->* . xexpr-element-list?)]
  [parse-markdown ((string?) (symbol?) . ->* . xexpr-element-list?)]
  [current-strict-markdown? parameter/c]))

(module+ test
  (require rackunit))

(define (xexpr-element-list? xs)
  (xexpr? `(dummy () ,@xs)))

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
  (when (equal? open close)
    (error 'chars-in-balanced "open and close chars must differ"))
  (pdo-seq (inner open close)
           #:combine-with (compose1 list->string flatten)))

(module+ test
  (check-equal? (parse-result (chars-in-balanced #\< #\>) "<yo <yo <yo>>>")
                "yo <yo <yo>>"))

;; This differs from `between` not just in the `(notFollowedBy
;; $space)` aspect -- which you could compose with open and supply to
;; between -- but more importantly because `p` needn't have a negative
;; check for close.  This uses (many1Till p close). Whereas `between`
;; is simply `(parser-one open (~> p) close)`
(define (enclosed open close p) ;; parser? parser? parser? -> (listof any/c)
  (try (pdo open
            (notFollowedBy $space)
            (xs <- (many1Till p close))
            (return xs))))

;; Parse contents of 'str' using 'parser' and return result, but,
;; restore the original state (input and position).
;; http://hackage.haskell.org/package/open-pandoc-1.5.1.1/docs/src/Text-Pandoc-Shared.html#parseFromString
(define (parse-from-string p str)
  (match-lambda
    [(and old-state (State old-inp old-pos))
     (match (p (State str old-pos))
       [(Consumed! (Ok result (? State? new-state) msg))
        (Consumed (Ok result old-state   msg))]
       [(Empty (Ok result (? State? new-state) msg))
        (Empty (Ok result old-state   msg))]
       ;;[(Consumed (Error msg)) __]
       ;;[(Empty (Error msg)) __])]))
       [x x])]))

;; Add this one to parsack itself?
(define (fail msg)
  (match-lambda
   [(and state (State inp pos))
    (Empty (Error (Msg pos inp (list (format "not ~a:" msg)))))]))

;; Creates a parser that, if `f?` returns true, fails; otherwise uses
;; `parser`. Useful for "fencing off" parts of a grammar. `f?` is
;; (-> boolean?), including parameter/c.
(define (parse-unless f? parser) ;(-> boolean?) parser? -> parser?
  (lambda (state)
    (if (f?)
        ((fail "") state)
        (parser state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A couple crude debugging tools to get "trace" of the parse.

(define-syntax (tr stx)
  (syntax-case stx ()
    [(_ parser)
     (with-syntax ([fn (format "~v" (syntax->datum #'parser))])
       #'(match-lambda
          [(and state (State inp pos))
           (printf "Try ~a\nat ~v on ~v\n"
                   fn
                   pos
                   (substring inp 0 (min (string-length inp) 40)))
           (parser state)]))]))

(define-syntax-rule (<OR> p ...)
  (tr (<or> (tr p) ...)))

;; Use to step through a part of the grammar that is (many1 p).
(define (parse-debug p s)
  (define-values (parsed more)
    (match (parse p s)
      [(Consumed! (Ok parsed (State more _) _)) (values parsed more)]
      [(Empty     (Ok parsed (State more _) _)) (values parsed more)]
      [x (parsack-error (~v x))]))
  (displayln "parsed:")
  (pretty-print parsed)
  (unless (equal? more "")
    (parse-debug p more)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is to process an entire Markdown document.
;; - Sets parameters like footnote number to 0.
;; - Expects \n not \r\n so when opening .md files use `#:mode 'text`
;;   or convert manually before calling parse-markdown.
;; - Appends a "\n\n" to `input` to simplify whole-document parsing.
(define (parse-markdown s [footnote-prefix-symbol (gensym)])
  (parameterize ([current-linkrefs (make-hash)]
                 [current-footnote-number 0]
                 [current-footnote-prefix footnote-prefix-symbol]
                 [current-footnotes (make-hash)]
                 [current-footnote-defs (make-hash)])
    (~>> (parse-markdown* (string-append s "\n\n"))
         resolve-refs
         append-footnote-defs)))

;; Use this internally to recursively parse fragments of Markdown
;; within the document. Does NOT set parameters. Does not append "\n"
;; to the string; up to caller to do so if required.
(define (parse-markdown* s)
  (~>> (parse-result $markdown s)
       normalize-xexprs))

;; For backward compatibility
(define (read-markdown [footnote-prefix-symbol (gensym)])
  (parse-markdown (port->string (current-input-port)) footnote-prefix-symbol))

;; Parameter to limit us to strict markdown (no customizations).
(define current-strict-markdown? (make-parameter #f))
;; A parser to make use of the parameter.
(define unless-strict (curry parse-unless current-strict-markdown?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Characters and tokens

(define space-chars " \t")
(define $space-char
  (<?> (oneOf space-chars) "space or tab"))

(define $sp
  (<?> (many $space-char)
       "zero or more spaces or tabs"))

(define $spnl
  (<?> (pdo-one $sp (optional (pdo-seq $newline $sp))
                (~> (return null)))
       "zero or more spaces, and optional newline plus zero or more spaces"))

(define special-chars "*_`&[]<!\\'\"-.")
(define $special-char (<?> (pdo-one (~> (oneOf special-chars)))
                           "special char"))

(define $escaped-char
  (<?> (pdo (char #\\)
            (option #\\
                    (satisfy (lambda (c)
                               (not (or (char-alphabetic? c)
                                        (char-numeric? c)))))))
       "escaped char"))

(define $normal-char
  (<?> (<or> $escaped-char
             (noneOf (~a space-chars special-chars "\n")))
       "normal char"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Strings

(define $non-indent-space
  (<?> (pdo (oneOfStrings "   " "  " " " "")
            (return null))
       "non-indent space"))

(define $indent
  (<or> (string "\t") (string "    ")))

(define $indented-line
  (pdo $indent
       $any-line))

(define $any-line
  (pdo (xs <- (manyTill $anyChar $newline))
       (return (list->string xs))))

(define $blank-line
  (try (pdo $sp $newline (return "\n"))))

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
  ;; (or/c string? symbol?) -> null
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

(define $html-comment
  (try (pdo (string "<!--")
            (xs <- (many1Till $anyChar (try (string "-->"))))
            (many $blank-line)
            (return `(!HTML-COMMENT () ,(list->string xs))))))

(define (html-trailing-space block?)
  (cond [block? (many $blank-line)]
        [else (return null)]))

(define (html-pre block?)
  (try (pdo (n+a <- (open-tag "pre"))
            (xs <- (manyTill $anyChar (close-tag "pre")))
            (html-trailing-space block?)
            (return (append n+a (list (list->string xs)))))))

;; Try to parse a matching pair of open/close tags like <p> </p>.
(define (html-element block?)
  (try (pdo (name+attributes <- (lookAhead $any-open-tag))
            (xs <- (balanced (open-tag (car name+attributes))
                             (close-tag (car name+attributes))
                             $inline
                             #:combine-with (lambda (open els _)
                                              (append* open els))))
            (html-trailing-space block?)
            (return xs))))
                        
(define (html-element/void block?)
  ;; -> (list symbol? (listof (list/c symbol? string?)))
  (try (pdo (char #\<)
            (name+attrs <- $html-tag+attributes)
            (optional (char #\/))
            (cond [(void-element? name+attrs) (optional (char #\/))]
                  [else (char #\/)])
            $spnl
            (char #\>)
            (html-trailing-space block?)
            (return name+attrs))))

(define $html/block (<or> $html-comment
                          (html-pre #t)
                          (html-element #t)
                          (html-element/void #t)))

(define $html/inline (<or> $html-comment
                           (html-pre #t)
                           (html-element #f)
                           (html-element/void #f)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inline
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used to disable parsing inline links.
(define ignore-inline-links? (make-parameter #f))

(define ($strong state)
  (_$strong state)) ;; defined after $inline

(define ($emph state)
  ($_emph state))   ;; defined after $inline and $strong

(define $in-backticks
  (try (pdo (os <- (many1 (char #\`)))
            (xs <- (many1Till $anyChar (try (string (list->string os)))))
            (return (string-trim (list->string xs))))))

(define $code
  (try (pdo (str <- $in-backticks)
            (lang <- (option #f (unless-strict $chars-in-brackets)))
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
  (try (pdo (char #\space)
            (char #\space)
            $sp
            $end-line
            (return `(br ())))))

(define $spaces->space
  (pdo (many1 $space-char)
       (return " ")))

(define $whitespace
  (<or> $line-break
        $spaces->space))

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

(define $footnote-ref
  (try (pdo (label <- $footnote-label)
            (return
             (let ([num (on-footnote-use! label)])
               `(sup () (a ([href ,(~a "#" (footnote-number->def-uri num))]
                            [name ,(footnote-number->use-uri num)])
                           ,(~a num))))))))

;;----------------------------------------------------------------------

(define $link-title
  (try (pdo $spnl
            (c <- (oneOf "'\""))
            (xs <- (manyTill (pdo-one (optional (char #\\)) (~> $anyChar))
                             (try (pdo-seq (char c) $sp $eof))))
            (return (list->string xs)))))

(define (source-url excludes)
  (pdo (xs <- (many (>> (optional (char #\\))
                        (<or> (noneOf (string-append " " excludes))
                              ;; (>> (notFollowedBy $link-title)
                              ;;     (char #\space))
                              ))))
       (return (list->string xs))))

(define $source
  (<or> (try (pdo (char #\<)
                  (xs <- (source-url ">\t\n"))
                  (char #\>)
                  (return xs)))
        (source-url "\t\n")))

(define $_source+title
  (pdo $sp
       (src <- $source)
       (tit <- (option "" $link-title))
       $sp
       $eof
       (return (list src tit))))

(define $source+title
  (<or> (try (>>= (chars-in-balanced #\( #\))
                  (curry parse-from-string $_source+title)))
        ;; handle e.g. [ref](/url(a).
        (pdo (>>= (pdo (xs <- (enclosed (char #\() (char #\)) $anyChar))
                       (return (list->string xs)))
                  (curry parse-from-string $_source+title)))))

(define $chars-in-brackets
  (chars-in-balanced #\[ #\]))

;; Used by both $link/explicit and $image/explicit
(define $explicit-link
  (try (pdo (label <- $chars-in-brackets)
            ;; NO $spnl here. Unlike reference links.
            (src+tit <- $source+title)
            (return (cons label src+tit)))))

;; Tries to parse as a reference link. Even if successful, returns
;; enough pices of the original input that it can be recontructed.
;; That way, if the reference link turns out to be undefined, we can
;; reparse the original text. Which believe it or not, is how markdown
;; is supposed to work: "[Foo]" and "[Foo][]" are links iff "Foo"
;; reference turns out to be defined, otherwise the brackets are to be
;; output.
;;
;; -> (list string?           ;label w/o its original brackets
;;          string?           ;text between the label and reference
;;          (or/c #f string?) ;reference, if any, w/o its original brackets
;;          string?)          ;sluggified ID, from ref if present and
;;                            ;non-blank, else from label
;;
;; Used by both $link/reference and $image/reference
(define $reference-link
  (try (pdo (label <- $chars-in-brackets)
            (x <- (option #f $sep+ref))
            (let* ([sep (match x [(cons x _) x] [#f ""])]
                   [ref (match x [(cons _ x) x] [#f #f])]
                   [id (match ref [(or #f "") label] [_ ref])])
              (return (list label sep ref id))))))

;; Parse a reference link's reference (the second set of braackets),
;; including the characters preceding it.
;;
;; -> (cons string?  ;before
;;          string?) ;reference
(define $sep+ref
  (try (pdo (btwn <- (pdo (cs <- (many (pdo (notFollowedBy (string "\n\n"))
                                            (oneOf " \t\n"))))
                          (return (list->string cs))))
            (ref <- $chars-in-brackets)
            (return (cons btwn ref)))))

(define $link/explicit
  (pdo (x <- $explicit-link)
       (return
        (match x
         [(list label src title)
          (define xs (parameterize ([ignore-inline-links? #t])
                       (normalize-xexprs
                        (parse-result (many $inline) label))))
          (match title
            ["" `(a ([href ,src])           ,@xs)]
            [t  `(a ([href ,src][title ,t]) ,@xs)])]))))

(define $link/reference
  (pdo (x <- $reference-link)
       (return
        (match x
          [(list label sep ref id)
           (delay ;postpone: only later will we know if `id` link defined
             (match (get-linkref id)
               [(cons src title)
                (define xs (parameterize ([ignore-inline-links? #t])
                             (parse-result (many $inline) label)))
                (match title
                  ["" `(a ([href ,src])            ,@xs)]
                  [t  `(a ([href ,src] [title ,t]) ,@xs)])]
               [#f (reparse-reference-link label sep ref)]))]))))

(define (reparse-reference-link label sep ref)
  ;; No reference definition was found. In that case, don't treat this
  ;; as a link to a bad URL. Instead markdown wants us to treat the
  ;; original input as if it wasn't a reference link in the first
  ;; place.
  ;;
  ;; Parse as normal $inline's inside the brackets, and put back in
  ;; the brackets from the original. In effect reparse as if we never
  ;; parsed the original input as a reference link.
  ;;
  ;; 1. Allow inline links to handle case of "[Handle [link] in
  ;; brackets]" where "link" is a defined reference.
  ;;
  ;; 2. Hack: If label ends wtich #\\, it was there to escape a
  ;; closing bracket, e.g. [foo\]. Remove here.  You may ask, why
  ;; didn't the escape prevent originally?  Good point, and someday
  ;; could fix that. But it works out because the label will include
  ;; the #\\, no reference will be found, therefore we get there.
  (parameterize ([ignore-inline-links? #f]) ;1
    (match label
      [(pregexp "^(.*?)\\\\?$" (list _ label)) ;2
       `(SPLICE "["
                ,@(parse-result (many $inline) label)
                "]"
                ,@(parse-result (many $inline) sep)
                ,@(match ref
                    [#f '("")]
                    [ref `("["
                           ,@(parse-result (many $inline) ref)
                           "]")]))])))

(define $link (<or> $link/explicit $link/reference))

(define $image/explicit
  (try (pdo (char #\!)
            (x <- $explicit-link)
            (return
             (match x
               [(list label src title)
                (match title
                  ["" `(img ([src ,src] [alt ,label]))]
                  [t  `(img ([src ,src] [alt ,label] [title ,t]))])])))))

(define $image/reference
  (try (pdo (char #\!)
            (x <- $reference-link)
            (return
             (match x
               [(list label sep ref id)
                (delay ;postpone: only later will we know if `id` link defined
                  (match (get-linkref id)
                    [(cons src title)
                     (match title
                       ["" `(img ([src ,src] [alt ,label]))]
                       [t  `(img ([src ,src] [alt ,label] [title ,t]))])]
                    [#f (match ref
                          [#f (~a "[" label "]")]
                          [_  (~a "[" label "]" sep "[" ref "]")])]))])))))

(define $image (<or> $image/explicit $image/reference))

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
             (unless-strict $smart-punctuation)
             $whitespace
             $end-line
             $code
             (<or> (4+ #\*) (4+ #\_))
             $strong
             $emph
             (unless-strict $footnote-ref)
             (parse-unless ignore-inline-links? $link)
             $image
             (parse-unless ignore-inline-links? $autolink) ;before html: faster
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

(define $blockquote-line
  (try (pdo-one $non-indent-space
                (char #\>)
                (optional (char #\space))
                (~> $any-line))))

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
  (try (pdo (xs <- (many1 (<or> (try $indented-line)
                                (try (pdo (bs <- (many (pdo $blank-line
                                                            (return ""))))
                                          (i <- $indented-line)
                                          (return
                                           (string-join `(,@bs ,i) "\n")))))))
            (many $blank-line)
            (return `(pre () (code () ,(string-join xs "\n")))))))

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
            (many $blank-line)
            (return (let ([text (string-join xs "\n")])
                      (match lang
                        ["" `(pre ()
                                  (code () ,text))]
                        [_  `(pre ([class ,(format "brush: ~a" lang)])
                                  (code () ,text))]))))))

(define $atx-heading
  (try (pdo (hs <- (many1 (char #\#)))
            $sp
            (xs <- (many1Till $inline $newline))
            $spnl
            (return (let ([sym (string->symbol (format "h~a" (length hs)))]
                          [id (xexprs->slug xs)])
                      `(,sym ([id ,id]) ,@xs))))))

(define $setext-heading
  (try (pdo (xs <- (many1Till $inline $newline))
            (c <- (oneOf "=-"))
            (many (char c))
            $newline
            (many1 $blank-line)
            (return (let ([sym (match c [#\= 'h1][#\- 'h2])]
                          [id (xexprs->slug xs)])
                      `(,sym ([id ,id]) ,@xs))))))

(define $heading (<or> $atx-heading $setext-heading))

(define $hr
  (try (pdo $non-indent-space
            (c <- (oneOf "*_-")) $sp
            (char c) $sp
            (char c) $sp
            (many (pdo-seq (char c) $sp))
            $newline
            (many $blank-line)
            (return `(hr ())))))

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
            (return
             (begin
               (on-footnote-def! label (string-join xs "\n"))
               "")))))

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

(define $reference-title
  ;; Not quite the same as $link-title. 1. Title may be grouped in
  ;; parens as well as single or double quotes. 2. Title ends with
  ;; $newline.
  (try (pdo $sp
            (beg <- (oneOf "('\""))
            (end <- (return (match beg [#\( #\)] [c c])))
            (xs <- (manyTill (pdo-one (optional (char #\\)) (~> $anyChar))
                             (try (pdo-seq (char end) $sp $newline))))
            (return (list->string xs)))))

(define $reference
  (try (pdo $non-indent-space
            (label <- $chars-in-brackets)
            (char #\:)
            $spnl
            (src <- (pdo (xs <- (many1Till $anyChar
                                           (<or> $space-char $newline)))
                         (return (list->string xs))))
            (title <- (option "" $reference-title))
            (many $blank-line)
            (return (begin
                      (add-linkref! label (cons src title))
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
            (bs <- (many $blank-line)) ;; "\n"
            (return (string-join (append xs bs) "")))))

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
             $verbatim/indent
             (unless-strict $verbatim/fenced)
             (unless-strict $footnote-def)
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
;; Reference links

(define current-linkrefs
  (make-parameter (make-hash))) ;(hash/c linkref? string?)

(define (resolve-refs xs) ;; xexpr-element-list? -> xexpr-element-list?
  ;; Walk the xexprs looking promises and force them.
  (define (do-xpr x)
    (match x
      [`(,tag ,attributes ,body ...)
       `(,tag ,attributes ,@(map do-xpr body))]
      [(? promise? x) (do-xpr (force x))] ;do-xpr in case nested promises
      [x x]
      ))
  (for/list ([x xs])
    (normalize (do-xpr x)))) ;normalize in case SPLICEs from promises

(define (normalize-linkref-id s)
  (regexp-replace* #px"\\s*\n" s " "))

(define (add-linkref! s uri) ;; string? string? -> any
  (hash-set! (current-linkrefs) (normalize-linkref-id s) uri))

(define (get-linkref s [warn? #f]) ;; string? [boolean?] -> string?
  (or (dict-ref (current-linkrefs) (normalize-linkref-id s) #f)
      (begin
        (and warn? (eprintf "Reference link not defined: ~v\n" s))
        #f)))

(module+ test
  (check-equal? (resolve-refs `((p () "hi")
                                ,(delay `(p () "there"))))
                '((p () "hi")
                  (p () "there"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Footnotes

(define current-footnote-prefix (make-parameter (gensym)))

(define current-footnote-number (make-parameter 0))

(define current-footnotes
  (make-parameter (make-hash))) ;(hash/c string? exact-positive-integer?)

;; Call when a footnote use is found, passing the [^label].  Returns a
;; number for the footnote.  Idempotent, i.e. if you call multi times
;; with same label returns same number.
(define (on-footnote-use! label) ;string? -> exact-positive-integer?
  (unless (hash-has-key? (current-footnotes) label)
    (hash-set! (current-footnotes) label (add1-footnote-number)))
  (hash-ref (current-footnotes) label))
  
(define (add1-footnote-number)
  (curry add1-param current-footnote-number))

(define (add1-param p) ;; parameter/c -> integer?
  (define v (add1 (p)))
  (p v)
  v)

(define current-footnote-defs
  (make-parameter (make-hash))) ;(hash/c string? string?)

;; Call when a footnote definition is found, passing the [^label] and
;; the list of xexprs parsed from the definition. Idempotent: Calling
;; again with same label is a no-op.
(define (on-footnote-def! label xs) ;string? (listof xexpr-list-element? ->void
  (unless (hash-has-key? (current-footnote-defs) label)
    (hash-set! (current-footnote-defs) label xs))
  (void))

;; Returns a `(div ([id "footnotes"]) (ol () (li () ___) ...))` where
;; each li is a footnote definition. To append to the end of the
;; parsed markdown xexprs.
(define (append-footnote-defs xs) ;; xexpr-element-list? -> xexpr-element-list?
  (append xs (get-footnote-defs-div)))

(define (get-footnote-defs-div) ;; -> (listof xexpr?)
  ;; Convert the current-foonotes hash to an alist so we can sort it.
  (define sorted-footnotes
    (sort (for/list ([(lbl num) (in-hash (current-footnotes))])
            (cons num lbl))
          < #:key car))
  (define lis
    (for/list ([(num lbl) (in-dict sorted-footnotes)])
      (match (hash-ref (current-footnote-defs) lbl)
        [#f ""]
        ;; Note: We want to tuck the ↩ return link at the end of the
        ;; last pargraph. It's actually simplest to store the footnote
        ;; definition as text, strip any trailing newlines, and append
        ;; our link in markdown format, then parse that markdown.
        [(pregexp "^(.*?)\n*$" (list _ s))
         (define md
           (~a s "&nbsp;[↩](#" (footnote-number->use-uri num) ")\n\n"))
         (define xs (parse-markdown* md))
         `(li ([id ,(footnote-number->def-uri num)]
                 [class "footnote-definition"])
                ,@xs)])))
  (match lis
    ['() '()]
    [lis `((div ([class "footnotes"])
                (ol () ,@lis)))]))

(define (footnote-number->use-uri n) ;; any/c -> string?
  (~a (current-footnote-prefix) "-footnote-" n "-return"))

(define (footnote-number->def-uri n) ;; any/c -> string?
  (~a (current-footnote-prefix) "-footnote-" n "-definition"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (parse-markdown @~a{+	this is a list item
;; 	indented with tabs

;; +   this is a list item
;;     indented with spaces

;; })

;;(parse $raw-list-item "+\tlist\n\ttabs\n\n")

;; (parse-markdown "list\n\ttabs\n\n")


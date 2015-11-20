#lang at-exp racket/base

(require racket/contract/base
         racket/dict
         racket/file
         racket/format
         racket/function
         racket/list
         racket/match
         racket/port
         racket/promise
         racket/string
         rackjure/threading
         xml/xexpr
         "entity.rkt"
         "html.rkt"
         "parsack.rkt"
         "xexpr.rkt"
         "xexpr2text.rkt")

(provide
 (contract-out
  [read-markdown (->* () (symbol?) xexpr-element-list?)]
  [parse-markdown (->* ((or/c string? path?)) (symbol?) xexpr-element-list?)]
  [current-strict-markdown? (parameter/c boolean?)]))

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
  (λ (in)
    (define in-str (open-input-string str))
    (port-count-lines! in-str)
    (p in-str)))

;; Add this one to parsack itself? (IIUC it's essentially $err with
;; ability to specify the message instead of it being '().)
(define (fail msg)
  (err (format "not ~a:" msg)))

;; Creates a parser that, if `f?` returns true, fails; otherwise uses
;; `parser`. Useful for "fencing off" parts of a grammar. `f?` is
;; (-> boolean?), including parameter/c.
(define (parse-unless f? parser) ;(-> boolean?) parser? -> parser?
  (λ (in) (if (f?) ($err in) (parser in))))

(define (getPosition)
  (λ (in)
    (define-values (r c pos) (port-next-location in))
    (Empty (Ok (list r c pos)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is to process an entire Markdown document.
;; - Sets parameters like footnote number to 0.
;; - Deletes all \r (including but not limited to \r\n -> \n)
;; - Appends a "\n\n" to `input` to simplify whole-document parsing.
(define (parse-markdown x [footnote-prefix-symbol (gensym)])
  (define-values (text source)
    (cond [(path? x) (values (file->string/no-cr x) x)]
          [else      (values x "<string>")]))
  (parameterize ([parse-source source]
                 [current-linkrefs (make-hash)]
                 [current-footnote-number 0]
                 [current-footnote-prefix footnote-prefix-symbol]
                 [current-footnotes (make-hash)]
                 [current-footnote-defs (make-hash)])
    (begin0
      (~>> (parse-markdown* (string-append text "\n\n"))
           resolve-refs
           append-footnote-defs)
      (OR-DEBUG:PRINT-RESULTS))))

(define (file->string/no-cr path)
  (regexp-replace* #rx"\r" (file->string path) ""))

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
  (<?> (pdo $sp (optional $newline) $sp
            (return null))
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
  (<?> (<or> (noneOf (~a space-chars special-chars "\n"))
             $escaped-char)
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

(define (walk-html x)
  (match x
    [`(pre . ,xs) `(pre ,@xs)]
    [`(!HTML-COMMENT . ,xs) `(!HTML-COMMENT ,@xs)]
    [`(,tag (,as ...) . ,es) `(,tag (,@as) ,@(map walk-html es))]
    [(? string? s) `(SPLICE ,@(parse-markdown* s))]
    [x x]))

(define $html/block
  (pdo (x <- $html-block-element)
       (many $blank-line)
       (return (~> x normalize-xexprs))))

;; This doesn't mean "parse only HTML inline elements", it means
;; "parse HTML elements in a markdown $inline context".
(define $html/inline
  (pdo (x <- $html-element) ;; allow e.g. "<i>x</i><table></table>"
       (return (~> x normalize-xexprs walk-html))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inline
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Used to disable parsing inline links.
(define ignore-inline-links? (make-parameter #f))

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

;; Setting user state for the last $str pos and val helps us do things
;; that would require look-behind in a regular expression.
(define $str
  (pdo (cs <- (many1 $normal-char))
       (val <- (return (list->string cs)))
       (pos <- (getPosition))
       (setState 'last-$str-pos pos)
       (setState 'last-$str-val val)
       (return val)))
  ;; (try (>>= (many1 $normal-char)
  ;;           (compose1 return list->string))))

(define $special
  (>>= $special-char
       (compose1 return (curry make-string 1))))

(define in-list-item? (make-parameter #f))
(define $end-line
  (try (pdo $newline
            (notFollowedBy $blank-line)
            ;; Allow a block HTML element to follow without a blank
            ;; line, e.g. "foo\n<table></table>"
            (notFollowedBy $html/block)
            (if (in-list-item?)
                (notFollowedBy $list-start)
                (return null))
            (return " "))))

(define $whitespace
  ;; 1. 2+ spaces followed by $end-line ==> <br>
  ;; 2. 1+ spaces ==> " "
  (pdo $space-char
       (<or> (try (pdo (many1 $space-char) $end-line (return `(br ())))) ;1
             (>> (many $space-char) (return " "))))) ;2

(define ($strong state)
  ($_strong state)) ;; defined after $inline

(define ($emph state)
  ($_emph state))   ;; defined after $inline and $strong

;;----------------------------------------------------------------------
;; smart punctuation

(define $smart-em-dash
  (>> (try (pdo (char #\-) (char #\-) (optional (char #\-))))
      (return 'mdash)))

(define $smart-en-dash
  (try (pdo (char #\-)
            (lookAhead (pdo (many (char #\space)) $digit))
            (return 'ndash))))

(define $smart-dashes (<or> $smart-em-dash $smart-en-dash))

(define $smart-prime ;; e.g. {6' tall} or {6'2" tall}
  (try (pdo (pos <- (getPosition))
            (last-$str-pos <- (getState 'last-$str-pos))
            (last-$str-val <- (getState 'last-$str-val))
            (char #\')
            (notFollowedBy $letter) ;not contraction e.g. {2's complement}
            ;; Following $str that was all digits?
            (cond [(and (equal? pos last-$str-pos)
                        last-$str-val
                        (for/and ([c (in-string last-$str-val)])
                          (char-numeric? c)))
                   (return 'prime)]
                  [else (fail "")]))))

(define $smart-apostrophe
  (pdo (char #\')
       (return 'rsquo))) ;; could use 'apos for HTML5?

(define (fail-in-quote-context x)
  (pdo (qc <- (getState 'quote-context))
       (cond [(eq? qc x) (fail (format "already in ~a quote" x))]
             [else (return null)])))

(define (fail-just-after-str)
  (pdo (pos <- (getPosition))
       (str-pos <- (getState 'last-$str-pos))
       (str-val <- (getState 'last-$str-val))
       (cond [(and (equal? pos str-pos)
                   (not (equal? (string-last-char str-val) #\()))
              (fail "just after $str")]
             [else (return null)])))

(define (string-last-char s)
  (define len (string-length s))
  (cond [(zero? len) ""]
        [else (string-ref s (sub1 len))]))

(define $single-quote-start
  (pdo (fail-in-quote-context 'single)
       (fail-just-after-str)
       (char #\')
       (lookAhead (<or> $alphaNum (char #\")))
       (return 'sdquo)))

(define $single-quote-end
  (try (>> (char #\')
           (notFollowedBy $alphaNum))))

(define $smart-quoted/single
  (try (pdo $single-quote-start
            (xs <- (withState (['quote-context 'single])
                     (many1Till $inline $single-quote-end)))
            (return `(SPLICE lsquo ,@xs rsquo)))))

(define $double-quote-start
  (pdo (fail-in-quote-context 'double)
       (fail-just-after-str)
       (char #\")
       (lookAhead (<or> $alphaNum (char #\')))
       (return 'ldquo)))

(define $double-quote-end
  (try (>> (char #\")
           (notFollowedBy $alphaNum))))

(define $smart-quoted/double
  (try (pdo $double-quote-start
            (xs <- (withState (['quote-context 'double])
                     (many1Till $inline $double-quote-end)))
            (return `(SPLICE ldquo ,@xs rdquo)))))

(define $smart-quoted (<or> $smart-quoted/single
                            $smart-quoted/double))

(define $smart-ellipses
  (<?> (pdo (oneOfStrings "..." " . . . " ". . ." " . . .")
            (return 'hellip))
       "ellipsis"))

(define $smart-punctuation
  (<or> $smart-quoted
        $smart-prime
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
                        (noneOf (string-append " " excludes)))))
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
;; reference turns out to be defined, otherwise they're just text with
;; brackets.
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

;; Parse a reference link's reference (the second set of brackets),
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
  ;; 2. Hack: If label ends with #\\, it was there to escape a
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

(define (img block? label src title)
  ;; boolean? string? string? (or/c #f string?) -> xexpr?
  (define img
    (match title
      ["" `(img ([src ,src] [alt ,label]))]
      [t  `(img ([src ,src] [alt ,label] [title ,t]))]))
  (cond [(and block? (not (current-strict-markdown?)))
         (define xs (parameterize ([ignore-inline-links? #f]) ;; want them
                      (parse-result (many $inline) label)))
         `(div ([class "figure"])
               ,img
               (p ([class "caption"]) ,@xs))]
        [else img]))

(define (image/explicit block?)
  (try (pdo (char #\!)
            (x <- $explicit-link)
            (return (apply img (cons block? x))))))

(define (image/reference block?)
  (try (pdo (char #\!)
            (x <- $reference-link)
            (return
             (match x
               [(list label sep ref id)
                (delay ;postpone: only later will we know if `id` link defined
                  (match (get-linkref id)
                    [(cons src title)
                     (img block? label src title)]
                    [#f (match ref
                          [#f (~a "[" label "]")]
                          [_  (~a "[" label "]" sep "[" ref "]")])]))])))))

(define $image/inline
  (<or> (image/explicit #f) (image/reference #f)))

(define $image/block
  (try (pdo-one (~> (<or> (image/explicit #t) (image/reference #t)))
                $newline
                (many $blank-line))))

(define (uri-host/port/path-char? c)
  (not (memq c '(#\newline #\space #\< #\>))))

(define $uri ;; -> pair?
  (try (pdo (scheme <- (oneOfStrings "https" "http" "ftp" "mailto"))
            (string "://")
            (addr <- (many1 (satisfy uri-host/port/path-char?)))
            (return
             (let ([href (string-append (list->string scheme) "://" (list->string addr))])
               (cons href href))))))

(define (email-char? c)
  (or (char-alphabetic? c)
      (char-numeric? c)
      (memq c '(#\. #\- #\_ #\+))))

(define $email ;; -> pair?
  (try (pdo (user <- (many1 (satisfy email-char?)))
            (char #\@)
            (host <- (many1 (satisfy email-char?)))
            (return
             (let* ([addr (string-append (list->string user) "@" (list->string host))]
                    [href (string-append "mailto:" addr)])
               (cons href addr))))))

(define $autolink
  (try (pdo (char #\<)
            (href+label <- (<or> $uri $email))
            (char #\>)
            (return (match-let ([(cons href label) href+label])
                      `(a ([href ,href]) ,label))))))

;; Idea from pandoc: To avoid perf probs, parse 4+ * or _ as literal
;; instead of attempting to parse as $emph or $strong.
(define $at-least-4-stars-or-underlines
  (try (pdo (c <- (oneOf "*_"))
            (char c) (char c) (char c)
            (xs <- (many (char c)))
            (return (string-append (make-string 4 c)
                                   (list->string xs))))))

(define $math-jax-inline
  (try (pdo (string "\\\\(")
            (xs <- (many1Till $anyChar (try (string "\\\\)"))))
            (return `(script ([type "math/tex"])
                             ,(list->string xs))))))

(define $math-jax-display
  (try (pdo (string "\\\\[")
            (xs <- (many1Till $anyChar (try (string "\\\\]"))))
            (return `(script ([type "math/tex; mode=display"])
                             ,(list->string xs))))))

(define $math-jax
  (<or> $math-jax-inline
        $math-jax-display))

(define $inline
  (<?> (<or> (unless-strict $math-jax)
             $str
             $whitespace
             $end-line
             (unless-strict $smart-punctuation)
             $code
             $at-least-4-stars-or-underlines
             $strong
             $emph
             (unless-strict $footnote-ref)
             (parse-unless ignore-inline-links? $link)
             $image/inline
             (parse-unless ignore-inline-links? $autolink) ;before html
             $html/inline
             $entity
             $special)
       "inline"))

;;; Have to define these after $inline

(define $_strong
  (pdo (fail-just-after-str)
       (cs  <- (lookAhead (oneOfStrings "**" "__")))
       (str <- (return (list->string cs)))
       (xs  <- (enclosed (string str) (try (string str)) $inline))
       (return `(strong () ,@xs))))

(define $_emph
  (pdo (fail-just-after-str)
       (c  <- (lookAhead (oneOf "*_")))
       (xs <- (enclosed (pdo (char c) (notFollowedBy (char c)))
                        (try (pdo (notFollowedBy $strong)
                                  (char c)
                                  (notFollowedBy $alphaNum)))
                        $inline))
       (return `(em () ,@xs))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Block
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is a common-prefix refactoring of what used to be three
;; parsers -- $para, $plain, and $setext-heading.
(define $setext-heading/para/plain
  (try
   (pdo (xs <- (many1 (pdo (notFollowedBy $newline) $inline)))
        (<or>
         ;; setext heading
         (try (pdo $newline
                   (c <- (oneOf "=-"))
                   (many (char c))
                   $newline
                   (many1 $blank-line)
                   (return (heading-xexpr (match c [#\= 'h1] [#\- 'h2]) xs))))
         ;; para or plain
         (pdo (ys <- (many $inline)) ;; \n may be $end-line
              (<or>
               ;; para
               (try (pdo $newline
                         (<or> (many1 $blank-line)
                               ;; Allow a block HTML
                               ;; element to follow without
                               ;; a blank line, e.g.
                               ;; "foo\n<table></table>"
                               (lookAhead $html/block))
                         (return `(p () ,@(append xs ys)))))
               ;; plain
               (pdo (optional $blank-line)
                    (return `(SPLICE ,@(append xs ys))))))))))

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
       (return (string-trim (list->string xs)))))

(define $fence-line-close
  (try (pdo-seq (string "```") $sp $newline)))

(define $verbatim/fenced
  (try (pdo (lang <- $fence-line-open)
            (xs <- (manyTill $any-line $fence-line-close))
            (many $blank-line)
            (return (let ([text (string-join xs "\n")])
                      (match lang
                        ["" `(pre ()
                                  (code () ,text))]
                        [_  `(pre ([class ,(format "brush: ~a" lang)])
                                  (code () ,text))]))))))

(define $trailing-hashes-before-newline
  (try (pdo $sp
            (many (char #\#))
            $sp
            (lookAhead $newline)
            (return ""))))

(define $atx-heading
  (try (pdo (hs <- (many1 (char #\#)))
            $sp
            (xs <- (many1Till (<or> $trailing-hashes-before-newline
                                    $inline)
                              $newline))
            $spnl
            (many $blank-line)
            (return (heading-xexpr (string->symbol (format "h~a" (length hs)))
                                   xs)))))

(define (heading-xexpr sym xs)
  (define id (xexprs->slug xs))
  (cond [(current-strict-markdown?) `(,sym ()         ,@xs)]
        [else                       `(,sym ([id ,id]) ,@xs)]))

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
            (many $blank-line)
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
            (xs <- (manyTill $anyChar $newline))
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
      [(cons (and x `(li () (p () . ,els))) '()) ;; last
       (list (cond [all-tight? `(li () ,@els)]
                   [else x]))]
      [(cons (and x `(li () (p . ,_))) more) ;; loose
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

(define $block
  (<?> (<or> $atx-heading
             $blockquote
             $verbatim/indent
             (unless-strict $verbatim/fenced)
             (unless-strict $footnote-def)
             (unless-strict $image/block)
             $reference
             $html/block
             $list
             $hr
             $setext-heading/para/plain)
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
  ;; Walk the xexprs looking for promises and force them.
  (define (do-xpr x)
    (match x
      [`(,tag ,attributes . ,body)
       `(,tag ,attributes ,@(map do-xpr body))]
      [(? promise? x) (do-xpr (force x))] ;do-xpr in case nested promises
      [x x]))
  (for/list ([x (in-list xs)])
    (normalize (do-xpr x)))) ;normalize in case SPLICEs from promises

(define (normalize-linkref-id s)
  (string-downcase (regexp-replace* #px"\\s*\n" s " ")))

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
      (match (hash-ref (current-footnote-defs) lbl #f)
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

#lang racket

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Characters and tokens

(define $space-char (oneOf " \t"))
(define $sp (many $space-char))
(define $spnl (parser-seq $sp (option "" (parser-seq $newline $sp))))

(define $special-char (oneOf "*_`&[]<!\\"))
(define $escaped-char (parser-one (char #\\) (~> $anyChar)))
(define $normal-char (<or> $escaped-char
                           (parser-one (notFollowedBy $special-char)
                                       (notFollowedBy $space-char)
                                       (notFollowedBy $newline)
                                       (~> $anyChar))))

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
(define $quoted (<or> (>>= (between (char #\")
                                    (char #\")
                                    (many (noneOf "\"")))
                           (compose1 return list->string))
                      (>>= (between (char #\')
                                    (char #\')
                                    (many (noneOf "'")))
                           (compose1 return list->string))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; HTML

(define $html-comment
  (try
   (parser-compose
    (x <- (between (string "<--")
                   (string "-->")
                   (many (parser-one (notFollowedBy (string "-->"))
                                     (~> $anyChar)))))
    (return `(html-comment () ,(list->string x))))))

(define $html-attribute
  (parser-compose
   (key <- (many1 (<or> $letter $digit)))
   $spnl
   (option "" (string "="))
   $spnl
   (val <- (<or> $quoted
                 (many1 (parser-seq (notFollowedBy $spaces)
                                    $anyChar))))
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
                   (xs <- (many (parser-one (notFollowedBy (close tag))
                                            (~> $inline))))
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

(define $one* (parser-seq (string "*") (<!> (string "*"))))
(define $emph* (try (parser-compose
                     $one*
                     (<!> (<or> $space-char $newline))
                     (xs <- (many1 (<or>
                                    $strong
                                    (parser-one (<!> (parser-seq $spnl $one*))
                                                (~> $inline)))))
                     $one*
                     (return `(em () ,@xs)))))
(define $one_ (parser-seq (string "_") (<!> (string "_"))))
(define $emph_ (try (parser-compose
                     $one_
                     (<!> (<or> $space-char $newline))
                     (xs <- (many1 (<or>
                                    $strong
                                    (parser-one (<!> (parser-seq $spnl $one_))
                                                (~> $inline)))))
                     $one_
                     (return `(em () ,@xs)))))
(define $emph (<or> $emph* $emph_))

(define $two* (parser-seq (string "**") (<!> (string "**"))))
(define $strong* (try (parser-compose
                       $two*
                       (<!> (<or> $space-char $newline))
                       (xs <- (many1 (parser-one (<!> (parser-seq $spnl $two*))
                                                 (~> $inline))))
                       $two*
                       (return `(strong () ,@xs)))))
(define $two_ (parser-seq (string "__") (<!> (string "__"))))
(define $strong_ (try (parser-compose
                       $two_
                       (<!> (<or> $space-char $newline))
                       (xs <- (many1 (parser-one (<!> (parser-seq $spnl $two_))
                                                 (~> $inline))))
                       $two_
                       (return `(strong () ,@xs)))))
(define $strong (<or> $strong* $strong_))

(define (ticks n)
  (parser-compose (string (make-string n #\`)) (<!> (char #\`))))
(define (between-ticks n)
  (try
   (between (ticks n)
            (ticks n)
            (parser-compose
             (xs <- (many1 (<or> (many1 (noneOf "`"))
                                 (parser-compose
                                  (<!> (ticks n))
                                  (xs <- (many1 (char #\`)))
                                  (return xs)))))
             (return `(code () ,(string-trim (list->string (append* xs)))))))))
(define codes (for/list ([n (in-range 10 0 -1)])
                (between-ticks n)))
(define $code (apply <or> codes))

(define $str (>>= (many1 $normal-char) (compose1 return list->string)))

(define $special (>>= (many1 $special-char) (compose1 return list->string)))

(define $br (try (parser-compose (char #\space) (char #\space) (char #\newline)
                                 (notFollowedBy $blank-line)
                                 (return `(br ())))))

(define $_spaces (>>= (many1 $space-char) (const (return " "))))

(define $end-line (try (parser-compose (optional (string " "))
                                       $newline
                                       (notFollowedBy $blank-line)
                                       (notFollowedBy $eof)
                                       (return " "))))

(define $line-break (parser-compose (string " ") $sp $end-line (return `(br))))

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

(define $footnote-label (try (parser-compose (char #\[)
                                             (char #\^)
                                             (xs <- (many (noneOf "]")))
                                             (char #\])
                                             (return (list->string xs)))))

(define $label (try (parser-compose (char #\[)
                                    (notFollowedBy (char #\^))
                                    (xs <- (many (noneOf "]")))
                                    (char #\])
                                    (return (list->string xs)))))

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
                       (tit <- (option "" (parser-one $sp (~> $title) $sp)))
                       (char #\))
                       (return (cons src tit))))

(define $explicit-link (try (parser-compose
                             (label <- $label)
                             (src+tit <- $source+title)
                             (return (match src+tit
                                       [(cons src tit)
                                        `(a ([href ,src]) ,label)])))))

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

(define $reference-link (try (parser-compose
                              (label <- $label)
                              $spnl
                              (href <- $label)
                              (return `(a ([href ,(ref:link (match href
                                                              ["" label]
                                                              [x x]))])
                                          ,label)))))

(define $link (<or> $explicit-link $reference-link))
(define $image (try (parser-compose (char #\!)
                                    (x <- $link)
                                    (return x)))) ;; to-do change to img

(define $html/inline (<or> $html-comment $html-element))

(define $inline (<or> $strong
                      $emph
                      $code
                      $br
                      $end-line
                      $_spaces ;not the parsack one
                      $footnote-ref
                      $link
                      $image
                      $autolink
                      $html/inline
                      $str
                      $entity
                      $special
                      ))

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
                                    (return `(@SPLICE ,@xs)))))

(define $blockquote (try (parser-compose
                          (xs <- (many1 $blockquote-line))
                          (ys <- (many (parser-one (notFollowedBy $blank-line)
                                                   (~> $any-line))))
                          (many $blank-line)
                          (return
                           (let* ([raw (string-join (append xs ys) "\n")]
                                  [xexprs (parse-markdown* raw)])
                             `(blockquote () ,@xexprs))))))
                           

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
                                      "\n")]
                               [xexprs (parse-markdown* s)])
                          (add-ref! (ref:note label) (~a "#" anchor))
                          `(div ([id ,anchor]
                                 [class "footnote-definition"])
                                ,@xexprs))))))

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
                         (return (list->string xs)))))

(define $raw-list-item (try (parser-compose
                             $list-start
                             (xs <- (many1 $list-line))
                             (_s <- (many $blank-line))
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
                  `(li () ,@(parse-markdown* raw)))))))

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
  (check-equal? (parse-markdown "- One.\n\n- Two.\n\n")
                '((ul () (li () (p () "One.")) (li () (p () "Two.")))))
  (check-equal? (parse-markdown "- One.\n- Two.\n")
                '((ul () (li () "One.") (li () "Two."))))
  (check-equal? (parse-markdown "  - One.\n\n  - Two.\n\n")
                '((ul () (li () (p () "One.")) (li () (p () "Two.")))))
  (check-equal? (parse-markdown "1. One.\n\n2. Two.\n\n")
                '((ol () (li () (p () "One.")) (li () (p () "Two."))))))

(define $html/block (<or> $html-comment $html-element))

(define $block (<or> $blockquote
                     $verbatim
                     $footnote-def
                     $reference
                     $html/block
                     $heading
                     $list
                     $hr
                     $para
                     $plain))

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
                      (cons this '())]
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
      [`((@SPLICE ,es ...) ,more ...)
       (loop (append es more))]
      [(cons this more)
       (cons this (loop more))]
      [(list) (list)])))

(module+ test
  (check-equal? (splice `((p () "A")
                          (@SPLICE "a" "b")
                          (p () "B")))
                `((p () "A") "a" "b" (p () "B")))
  (check-equal? (normalize `(p () "a" "b" (@SPLICE "c" "d") "e" "f"))
                `(p () "abcdef"))
  (check-equal? (normalize `(p () "a" (@SPLICE "b" (@SPLICE "c") "d") "e"))
                `(p () "abcde")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This is to process an entire Markdown docuemnt.
;; Sets parameters like footnote nubmer to 0.
(require rackjure/threading)
(define (parse-markdown s [footnote-prefix-symbol (gensym)])
  (parameterize ([current-refs (make-hash)]
                 [footnote-number 0]
                 [footnote-prefix footnote-prefix-symbol])
    (~>> (parse-markdown* s)
         normalize-xexprs
         resolve-refs)))

;; Use this for fragments of Markdown within the document (doesn't set
;; parameters).
;; Appends a "\n" to `input` to simplify whole-docuemnt parsing.
(define (parse-markdown* s)
  (parse-result $markdown (string-append s "\n")))

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

(module+ test
  ;; No input should ever cause a parse error or non-termination.
  ;; i.e. Random text is itself a valid Markdown format file.
  (for ([i 5])
    (check-not-exn (lambda () (parse-markdown (random-doc 50))))))

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

EOF
)

;; (require racket/trace)
;; (trace $blockquote
;;        $verbatim
;;        $reference
;;        $html/block
;;        $heading
;;        $list
;;        $hr
;;        $para
;;        $plain)

(require racket/runtime-path)
(define-runtime-path test.md "test/test.md")
;; (pretty-print (parse-markdown (file->string test.md)))

;; (pretty-print (parse-markdown input 'foo))

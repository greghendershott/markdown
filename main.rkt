#lang rackjure

(require xml (prefix-in h: html))

(provide
 (contract-out [read-markdown (-> (listof xexpr?))]
               [current-allow-html? (parameter/c boolean?)]
               [current-show-linkrefs-as-footnotes? (parameter/c boolean?)]
               [display-xexpr ((xexpr?) (0 #f) . ->* . any)]))

(module+ test
  (require rackunit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Allow literal HTML in the markdown file to be passed through to the
;; output? If #f then the HTML is escaped.
(define current-allow-html? (make-parameter #t))

;; Show linkrefs as footnotes?
(define current-show-linkrefs-as-footnotes? (make-parameter #f))

;; Returns (listof xexpr?) that may be spliced into a 'body element --
;; i.e. `(html () (head () (body () ,@(read-markdown))))
(define (read-markdown) ;; -> (listof xexpr?)
  (parameterize ([current-refs (make-hash)])
    (~> (read-blocks)
        resolve-refs
        remove-br-before-blocks)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; link refs

(define current-refs (make-parameter (make-hash)))

(define (resolve-refs xs) ;; (listof xexpr?) -> (listof xexpr?)
  ;; Walk the xexprs looking for 'a elements whose 'href attribute is
  ;; symbol?, and replace with hash value. Same for 'img elements'
  ;; 'src attributes.
  (define (get-attr alist k)
    (define v (assoc k alist))
    (and v (cadr v)))
  (define (set-attr alist k v)
    (dict-set alist k (list v))) ;put in list b/c dict-set on list not alist
  (define (resolve attrs name body)
    (define uri (get-attr attrs name))
    (cond [(and uri (symbol? uri))
           `(a ,(set-attr attrs name (get-ref uri)) ,@body)]
          [else `(a ,attrs ,@body)]))
  (define (do-xpr xs)
    (match xs
      [(list tag (list attrs ...) body ...)
       (match tag
         ['a   (resolve attrs 'href body)]
         ['img (resolve attrs 'src body)]
         [else `(,tag ,attrs ,@(map do-xpr body))])]
      [(list tag body ...) `(,tag ,@(map do-xpr body))]
      [else xs]))
  (for/list ([x xs])
    (do-xpr x)))

(define (add-ref! name uri) ;; symbol? string? -> any
  (hash-set! (current-refs) name uri))

(define (get-ref name)
  (or (dict-ref (current-refs) name #f)
      (begin (log-warning "Linkref '~a' not resolved.\n" name) "")))

(module+ test
  (check-equal? (parameterize ([current-refs (make-hash)])
                  (add-ref! 'foo "bar")
                  (resolve-refs '((a ([href foo]) "foo"))))
                '((a ((href "bar")) "foo"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block level

(define (read-blocks)
  (let loop ([xs '()])
    (define x (block-level))
    (cond [x (loop (append xs x))]
          [else xs])))

(define (remove-br-before-blocks xs)
  (match xs
    [(list) (list)]
    [(list (list 'br) (and next (list (or 'blockquote 'pre) _ ...)) more ...)
     (cons next (remove-br-before-blocks more))]
    [(list x more ...)
     (cons x (remove-br-before-blocks more))]))

(module+ test
  (check-equal?
   (remove-br-before-blocks
    '((br) (pre) (br) (p) (br) (blockquote) (br)))
   '((pre) (br) (p) (blockquote) (br))))

(define (block-level) ;; -> (or/c #f list?)
  (or (heading-block)
      (code-block-indent)
      (code-block-backtick)
      (blockquote)
      (hr-block) ;; must go BEFORE list block
      (list-block)
      (linkref-block)
      (other)))

(define (hr-block)
  (match (try #px"^(?:[*-] ?){3,}\n+")
    [(list _) `((hr))]
    [else #f]))

;; Lists and sublists. Uff da.
;;
;; 1. A ul is one or more li.
;;
;; 2. An li is a marker (e.g. "-"), followed by li-contents, followed
;; by another marker at the same level (or the end).
;;
;; 3. li-contents is list of either:
;;
;; - Some text (which should be run through intra-block processing).
;;
;; - Another ul (indicated by a marker (e.g. "-")) with more
;; indentation). Recursively do 1.

(define ulm "[*+-]")
(define olm "\\d+[.]")
(define marker (str "(?:" ulm "|" olm ")"))

;; Look for an entire list, including any sublists.
(define (list-block)
  (define px (pregexp (str "^"
                           "[ ]{0,3}" marker ".+?" "\n{2,}"
                           "(?:$|(?=\\S))"
                           "(?![ \t]*" marker "[ \t]+)" ;; not another one
                           )))
  (match (try px)
    [(list text) (list (do-list text))]
    [(var x) #f]))

;; Process an entire list level, and recursively process any
;; sub-lists.
(define (do-list s)
  (define xs
    (~>>
     ;; If string ends in > 1 \n, set it to just 1. See below.
     (regexp-replace #px"\n{2,}$" s "\n")
     ;; Split the string into list items.
     (regexp-split (pregexp (str "(?<=^|\n)" marker "\\s+")))
     ;; Remove the "" left by regexp-split
     (filter (negate (curry equal? "")))))
  (define first-marker (match s
                         [(pregexp (str "^(" marker ")") (list _ x)) x]))
  (define tag
    (match first-marker
      [(pregexp ulm) 'ul]
      [else 'ol]))
  `(,tag
    ,@(for/list ([x xs])
        (match x
          ;; List item with a sublist?
          [(pregexp (str "^(.+?)\\s*" "(?<=\n)" "(\\s+ " marker ".+)$")
                    (list _ text sublist))
           `(li ,@(intra-block text) ,(do-list (outdent sublist)))]
          [else
           (match x
             ;; If the item ends in 2+ \n, nest the text in a
             ;; 'p element to get a space between it and the
             ;; next item. (We stripped \n\n from the very last
             ;; item, above.)
             [(pregexp "^(.*?)\n{2}$" (list _ text))
              `(li (p ,@(intra-block text)))]
             ;; Otherwise just goes directly in the 'li element.
             [(pregexp "^(.*?)\n*$" (list _ text))
              `(li ,@(intra-block text))])]))))

(module+ test
  (check-equal? (do-list (str #:sep "\n"
                              "- Bullet 1"
                              ""
                              "- Bullet 2"
                              "  - Bullet 2a"
                              "  - Bullet 2b"
                              "    - Bullet 2bi"
                              "- Bullet 3"
                              "  - Bullet 3a"
                              "- Bullet 4"
                              "  continued"
                              "- Bullet 5"
                              ))
                '(ul (li (p "Bullet 1"))
                     (li
                      "Bullet 2"
                      (ul (li "Bullet 2a")
                          (li "Bullet 2b"
                              (ul (li "Bullet 2bi")))))
                     (li "Bullet 3"
                         (ul (li "Bullet 3a")))
                     (li "Bullet 4   continued")
                     (li "Bullet 5")))

  (check-equal? (do-list (str #:sep "\n"
                              "1. One"
                              "  1. One / One"
                              "  2. One / Two"
                              "2. Two"
                              "  1. Two / One"
                              "  2. Two / Two"
                              ""))
                '(ol
                  (li "One" (ol (li "One / One")
                                (li "One / Two")))
                  (li "Two" (ol (li "Two / One")
                                (li "Two / Two"))))))

(define (outdent s)
  (match s
    [(pregexp "^(\\s*)(.*)$" (list _ lead rest))
     (regexp-replace* (pregexp (str "\n" lead)) rest "\n")]))

(module+ test
  (check-equal?
   (outdent (str #:sep "\n"
                 "  Indent"
                 "  Indent"
                 "    More"
                 ""
                 "    More"
                 "  Indent"
                 "  Indent"))
   (str #:sep "\n"
        "Indent"
        "Indent"
        "  More"
        ""
        "  More"
        "Indent"
        "Indent")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (heading-block)
  (or (hash-heading-block)
      (equal-heading-block)
      (hyphen-heading-block)))

(define (hash-heading-block) ;; -> (or/c #f list?)
  (match (try #px"^\\s*(#+) ([^\n]+)\n\n")
    [(list _ pounds text)
     (define tag (~> (str "h" (string-length pounds))
                     string->symbol))
     `((,tag ,@(~> text intra-block)))]
    [else #f]))
              
(define (equal-heading-block) ;; -> (or/c #f list?)
  (match (try #px"^([^\n]+)\n={3,}\n{2,}?")
    [(list _ text) `((h1 ,@(~> text intra-block)))]
    [else #f]))

(define (hyphen-heading-block) ;; -> (or/c #f list?)
  (match (try #px"^([^\n]+)\n-{3,}\n{2,}?")
    [(list _ text) `((h2 ,@(~> text intra-block)))]
    [else #f]))

(module+ test
  (check-false (with-input-from-string "Some normal text.\n" heading-block))
  (check-equal?
   (with-input-from-string "# Hi there\n\nNot part of header" heading-block)
   '((h1 "Hi there")))
  (check-equal?
   (with-input-from-string "## Hi there\n\nNot part of header" heading-block)
   '((h2 "Hi there")))
  (check-equal?
   (with-input-from-string "Hi there\n===\n\nNot part of header" heading-block)
   '((h1 "Hi there")))
  (check-equal?
   (with-input-from-string "Hi there\n---\n\nNot part of header" heading-block)
   '((h2 "Hi there"))))

(define (code-block-indent) ;; -> (or/c #f list?)
  (match (try #px"^(    [^\n]*\n)+(?:$|\n)")
    [(list code _)
     `((pre (code ,(~> code
                       (nuke-all #px"^    ")
                       (nuke-all #px"\n    " "\n")
                       (nuke-all #px"\n+$")))))]
    [else #f]))

(define (code-block-backtick) ;; -> (or/c #f list?)
  (match (try #px"^```(.*?)\\s*\n(.*?\n)```\n")
    [(list _ lang code)
     `((pre (code ,@(match lang
                      ["" '()]
                      [else `(([class ,(str "brush: '" lang "';")]))])
                  ,(~> code (nuke-all #px"\n+$")))))]
    [else #f]))

(define (blockquote) ;; -> (or/c #f list?)
  (match (try #px"^> (.+?\n\n)+?")
    [(list _ bq)
     `((blockquote (p ,@(~> bq 
                            (nuke-all #px"\n+$" "")
                            (nuke-all #px"\n> " " ")
                            (nuke-all #px"\n" " ")
                            intra-block))))]
    [else #f]))

(define (linkref-block)
  ;; - Square brackets containing the link identifier (optionally
  ;;   indented from the left margin using up to three spaces);
  ;;
  ;; - followed by a colon;
  ;;
  ;; - followed by one or more spaces (or tabs);
  ;;
  ;; - followed by the URL for the link;
  ;;
  ;; - optionally followed by a title attribute for the link, enclosed
  ;;   in double or single quotes, or enclosed in parentheses.
  (match (try (pregexp (str "^"
                            "[ \t]{0,3}\\[(.+?)\\]:"
                            "[ \t]{1,}(\\S+)"
                            "\\s+[\"'(](.+?)[\"')]"
                            "\\s*\n+")))
    [(list _ refname uri title)
     (add-ref! (string->symbol refname) uri)
     (cond [(current-show-linkrefs-as-footnotes?)
            `((p "[" ,refname "]" (em ,title) ": " (a ([href ,uri]) ,uri)))]
           [else `("")])]
    [else #f]))

(module+ test
  (define-syntax-rule (chk str)
    (check-equal?
     (parameterize ([current-show-linkrefs-as-footnotes? #t])
       (with-input-from-string (string-append str "\n") linkref-block))
     '((p "[" "foo" "]" (em "Optional Title Here") ": "
          (a ((href "http://example.com/")) "http://example.com/")))))
  (chk "[foo]: http://example.com/  \"Optional Title Here\"")
  (chk "   [foo]:   http://example.com/     \"Optional Title Here\"")
  (chk "[foo]: http://example.com/  'Optional Title Here'")
  (chk "[foo]: http://example.com/  (Optional Title Here)")
  )

(define (other) ;; -> (or/c #f list?)
  (match (try #px"^(.+?)\n\n")
    [(list _ text)
     `((p ,@(intra-block text)))]
    [else
     (let ([s (read-line (current-input-port) 'any)])
       (and (not (eof-object? s))
            `(,@(intra-block (str s "\n")))))]))

(define (try re)
  (define xs (regexp-try-match re (current-input-port)))
  (and xs (map (lambda (x)
                 (and x (bytes->string/utf-8 x)))
               xs)))

(define (nuke-all s re [new ""])
  (regexp-replace* re s new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intra-block

(define (intra-block s) ;; s -> (listof xexpr?)
  ;; Look for formatting within a block
  (~> s
      list
      escape ;; before everything
      code ;; before everything else
      space&space&newline->br
      newlines->spaces
      html
      image
      link
      auto-link
      bold
      italic
      dashes
      remove-br-before-blocks
      unescape
      ))

(module+ test
  (check-equal? (intra-block "The expression `2 * foo_bar`")
                '("The expression " (code ([class "inline-code"])
                                          "2 * foo_bar"))))

;; `replace` is the workhorse for intra-block matching. It's in the
;; same spirit as regexep-replace*, but instead of just strings, it
;; deals with lists of xexprs (i.e. string or list).  It looks for
;; patterns in anything that is still a string?, and possibly breaks
;; the string into an xexpr?.  Subsequent calls to `replace` operate
;; only on elements that remained string?s.
;;
;; Given a (listof xexpr?) and a regexp, runs `regexp-match*` on each
;; xexpr that is a string?.  For each match that succeeds, calls `f`
;; with the results, and replaces the string with what `f` returns.
;; Uses `regexp-match` with #:gap-select? #t, so taht non-matches are
;; also returned. Instead of passing those to `f`, it simply keeps
;; them (unless they are "", in which case they're deleted).
(define (replace xs px f)
  (append*
   (map (lambda (x)
          (cond [(string? x)
                 (filter-map
                  values
                  (for/list ([x (in-list (regexp-match* px x
                                                        #:match-select values
                                                        #:gap-select? #t))])
                    (match x
                      [(? list? x) (apply f x)]
                      ["" #f]
                      [else x])))]
                [else (list x)]))
        xs)))

(define (escape xs)
  (replace xs "\\\\(.)" (lambda (_ x) (list 'ESCAPE x))))

(define (unescape xs)
  (for/list ([x xs])
    (match x
      [(list 'ESCAPE text) text]
      [else x])))

(module+ test
  (check-equal? (~> '("\\`not code`") escape)
                '((ESCAPE "`") "not code`"))
  (check-equal? (~> '("\\`not code`") escape unescape)
                '("`" "not code`")))


(define (code xs)
  (define (code-xexpr _ x) `(code ([class "inline-code"]) ,x))
  (~> xs
      (replace #px"`` ?(.+?) ?``" code-xexpr)
      (replace #px"`(.+?)`" code-xexpr)))

(module+ test
  ;; See http://daringfireball.net/projects/markdown/syntax#code
  (check-equal? (code '("This is some `inline code` here"))
                '("This is some "
                  (code ([class "inline-code"]) "inline code")
                  " here"))
  (check-equal? (code '(" `` ` `` "))
                '(" " (code ([class "inline-code"]) "`") " "))
  (check-equal? (code '(" `` `foo` ``"))
                '(" " (code ([class "inline-code"]) "`foo`")))
  (check-equal? (code '("``There is a literal backtick (`) here.``"))
                '((code ([class "inline-code"])
                        "There is a literal backtick (`) here."))))

(define (html xs)
  (define (elements->element xs)
    (make-element #f #f '*root '() xs))
  (cond [(current-allow-html?)
         ;; We want to splice in the xexprs, not nest them in some
         ;; dummy parent. That's the reason for the extra level using
         ;; `box`, followed by the `list`-ing of non-boxed elements,
         ;; and finally the append*.
         (~>>
          (replace xs #px"<.+?>.*</\\S+?>"
                   ;; Although using a regexp to identify HTML text, we
                   ;; let read-html-as-xml do the real work oarsing it:
                   (lambda (x)
                     (box (parameterize ([permissive-xexprs #t])
                            (~> (open-input-string x)
                                h:read-html-as-xml
                                elements->element
                                xml->xexpr
                                cddr)))))
          (map (lambda (x)
                 (cond [(box? x) (unbox x)]
                       [else (list x)])))
          (append*))]
        [else xs])) ;; xexpr->string automatically escapes string xexprs

(module+ test
  (check-equal?
   (html '("Here is a <span class='foo'>text</span> element."))
   '("Here is a " (span ((class "foo")) "text") " element."))
  ;; Confirm it works fine with \n in middle of <tag>
  (check-equal?
   (html '("<span\n style='font-weight:bold;'>span</span>"))
   '((span ((style "font-weight:bold;")) "span")))
  )

(define (space&space&newline->br xs)
  (replace xs #px"  \n" (lambda (_) `(br))))

(define (newlines->spaces xs)
  (for/list ([x (in-list xs)])
    (cond [(string? x) (regexp-replace* #px"\n" x " ")]
          [else x])))

(define (image xs)
  (~> xs
      (replace #px"!\\[(.*?)\\]\\(([^ ]+)(\\s+\"(.+?)\"\\s*)?\\)" ;normal
           (lambda (_ alt src __ title)
             `(img ([alt ,alt] [src ,src] [title ,(or title "")]))))
      (replace #px"!\\[(.*?)\\]\\[([^ ]+)\\]" ;reflink
           (lambda (_ alt src)
             `(img ([alt ,alt] [src ,(string->symbol src)]))))))

(module+ test
  (check-equal? (image '("![Alt text](/path/to/img.png)"))
                '((img ((alt "Alt text")
                        (src "/path/to/img.png")
                        (title "")))))
  (check-equal? (image '("![Alt text](/path/to/img.png \"Title\")"))
                '((img ((alt "Alt text")
                        (src "/path/to/img.png")
                        (title "Title")))))
  (check-equal? (image '("![Alt text][1]"))
                '((img ((alt "Alt text")
                        (src |1|))))))

(define (link xs)
  (~> xs
      (replace #px"\\[(.*?)\\][ ]{0,1}\\((.+?)\\)" ;normal
               (lambda (_ text href)
                 `(a ([href ,href]) ,text)))
      (replace #px"\\[(.+?)\\][ ]{0,1}\\[(.*?)\\]" ;reflink
               (lambda (_ text href)
                 (let ([href (match href ["" text][else href])])
                   `(a ([href ,(string->symbol href)]) ,text))))))

(module+ test
  (check-equal? (link '("[Google](http://www.google.com/)"))
                '((a ((href "http://www.google.com/")) "Google")))
  (check-equal? (link '("[Google] (http://www.google.com/)"))
                '((a ((href "http://www.google.com/")) "Google")))
  (check-equal? (link '("[Google][1]"))
                '((a ((href |1|)) "Google")))
  (check-equal? (link '("[Google][]"))
                '((a ((href Google)) "Google")))
  (check-equal? (link '("[Google] [1]"))
                '((a ((href |1|)) "Google"))))

(define (auto-link xs)
  (define (a _ uri)
    `(a ([href ,uri]) ,uri))
  (~> xs
      (replace #px"<(http.+?)>" a)
      (replace #px"<(www\\..+?)>" a)
      (replace #px"<([^@]+?@[^@]+?)>"
               (lambda (_ email) `(a ([href ,(str "mailto:" email)])
                                ,email)))
      (replace #px"<(.+\\.(?:com|net|org).*)>" a)))

(module+ test
  (check-equal?
   (auto-link '("<http://www.google.com/path/to/thing>"))
   '((a ((href "http://www.google.com/path/to/thing")) "http://www.google.com/path/to/thing")))
  (check-equal?
   (auto-link '("<www.google.com/path/to/thing>"))
   '((a ((href "www.google.com/path/to/thing")) "www.google.com/path/to/thing")))
  (check-equal?
   (auto-link '("<google.com/path/to/thing>"))
   '((a ((href "google.com/path/to/thing")) "google.com/path/to/thing")))
  (check-equal? (auto-link '("<foo@bar.com>"))
                '((a ((href "mailto:foo@bar.com")) "foo@bar.com"))))

;; NOTE: Tricky part here is that `_` is considered a \w word char but
;; `*` is not. Therefore using \b in pregexp works for `_` but not for
;; `*`. Argh.
;;
;; Instead of leading \\b we need to use (?<![*\\w])
;; Instead of trailing \\b we need to use (?![*\\w])
(define word-boundary-open "(?<![*\\w])")
(define word-boundary-close "(?![*\\w])")

(define (bold xs)
  (replace xs (pregexp (str word-boundary-open    ;not \\b
                            "[_*]{2}(.+?)[_*]{2}"
                            word-boundary-close)) ;not \\b
           (lambda (_ x) `(strong ,x))))

(module+ test
  (check-equal? (bold '("no __YES__ no __YES__"))
                '("no " (strong "YES") " no " (strong "YES")))
  (check-equal? (bold '("no **YES** no **YES**"))
                '("no " (strong "YES") " no " (strong "YES")))
  (check-equal? (bold '("2*3*4"))
                '("2*3*4")))

(define (italic xs)
  (replace xs (pregexp (str word-boundary-open    ;not \\b
                            "(?<!\\\\)[_*]{1}(.+?)[_*]{1}"
                            word-boundary-close)) ;not \\b
           (lambda (_ x) `(em ,x))))

(module+ test
  (check-equal? (italic '("no _YES_ no _YES_"))
                '("no " (em "YES") " no " (em "YES")))
  (check-equal? (italic '("no *YES* no *YES*"))
                '("no " (em "YES") " no " (em "YES")))
  (check-equal? (italic '("no_no_no"))
                '("no_no_no"))
  (check-equal? (italic '("_YES_ no no_no _YES_YES_ _YES YES_"))
                '((em "YES") " no no_no " (em "YES_YES") " " (em "YES YES")))
  (check-equal? (intra-block "\\_text surrounded by literal underlines\\_")
                '("_" "text surrounded by literal underlines" "_"))
  (check-equal? (intra-block "\\*text surrounded by literal asterisks\\*")
                '("*" "text surrounded by literal asterisks" "*")))

(define (dashes xs)
  (~> xs
      (replace #px" -- "
               (lambda (_) '(span " " ndash " ")))
      (replace #px"\\b--\\b"
               (lambda (_) '(span mdash)))))

(module+ test
  (check-equal? (dashes '("This -- section -- is here and this--is--here."))
                '("This" (span " " ndash " ") "section" (span " " ndash " ")
                  "is here and this" (span mdash) "is" (span mdash) "here.")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; display-xexpr

;; xexpr->string does too little formatting, and display-xml does too
;; much.  This is the warm bowl of porridge.

(define (display-xexpr x [indent 0][pre-indent #f])
  (define escape-table #rx"[<>&]")
  (define escape-attribute-table #rx"[<>&\"]")

  (define (replace-escaped s)
    (case (string-ref s 0)
      [(#\<) "&lt;"]
      [(#\>) "&gt;"]
      [(#\&) "&amp;"]
      [(#\") "&quot;"]))

  (define (escape x table)
    (regexp-replace* table x replace-escaped))

  (define (do tag ks vs body)
    (define indent-str (make-string (or pre-indent indent) #\space))
    (cond [(and (empty? ks) (empty? body))
           (printf "\n~a<~a />" indent-str tag)]
          [else
           (printf "\n~a<~a" indent-str tag)
           (for ([k ks]
                 [v vs])
             (printf " ~a=\"~a\"" k (escape v escape-attribute-table)))
           (printf ">")
           (for ([b body])
             (display-xexpr b
                            (+ 1 indent)
                            (if (or pre-indent (eq? tag 'pre)) 0 #f)))
           (printf "</~a>" tag)]))

  (match x
    [(list (? symbol? tag) (list (list ks vs) ...) els ...) (do tag ks vs els)]
    [(list tag els ...) (do tag '() '() els)]
    [(? string? x) (~> x (escape escape-table) display)]
    [(? symbol? x) (~> (format "&~a;" x) display)]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Unit test: Compare to static file.

(module+ test
  (require racket/runtime-path)

  (define-runtime-path test.md "test/test.md")
  (define xs (parameterize ([current-allow-html? #t])
               (with-input-from-file test.md read-markdown)))

  (define-runtime-path test.css "test/test.css")
  (define style `(link ([href ,(path->string test.css)]
                        [rel "stylesheet"]
                        [type "text/css"])))

  ;; Reference file. Check periodically.
  (define-runtime-path test.html "test/test.html")

  (define test.out.html (build-path (find-system-path 'temp-dir)
                                    "test.out.html"))

  (with-output-to-file test.out.html #:exists 'replace
                       (lambda ()
                         (~> `(html (head () ,style)
                                    (body () ,@xs))
                             display-xexpr)))

  (check-equal? (system/exit-code (str #:sep " "
                                       "diff" test.out.html test.html))
                0)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Unit tests: EOF
;;
;; Boundary case for bugs is things appearing at the very EOF.

(module+ test
  (define-syntax-rule (check-eof str xpr)
    (check-equal? (with-input-from-string str read-markdown) xpr))
  ;; List
  (check-eof "- Bullet 1\n- Bullet 2\n\n"
             '((ul (li "Bullet 1") (li "Bullet 2"))))
  ;; List
  (check-eof "- Bullet 1\n  - Bullet 1a\n- Bullet 2\n  - Bullet 2a\n\n"
             '((ul (li "Bullet 1" (ul (li "Bullet 1a")))
                   (li "Bullet 2" (ul (li "Bullet 2a"))))))
  ;; Header
  (check-eof "# Header 1\n\n"
             '((h1 "Header 1")))
  ;; Code block: ticks
  (check-eof "```\nCode block\n```\n"
             '((pre (code "Code block"))))
  ;; Code block: indent
  (check-eof "    Code block\n"
             '((pre (code "Code block"))))
  ;; Blockquote
  (check-eof "> Block quote here\n\n"
             '((blockquote (p "Block quote here"))))
  ;; hr
  (check-eof "---\n"
             '((hr)))
  ;; Linkref
  (parameterize ([current-show-linkrefs-as-footnotes? #t])
    (check-eof "An [example link][0]\n\n[0]: http://www.example.com/ \"Improbable Research\"\n"
               '((p "An " (a ((href "http://www.example.com/"))
                             "example link"))
                 (p "[" "0" "]" (em "Improbable Research") ": "
                    (a ((href "http://www.example.com/"))
                       "http://www.example.com/")))))
  (parameterize ([current-show-linkrefs-as-footnotes? #f])
    (check-eof "An [example link][0]\n\n[0]: http://www.example.com/ \"Improbable Research\"\n"
               '((p "An " (a ((href "http://www.example.com/"))
                             "example link"))
                 "")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Main

(module+ main
  (~> `(html (head ())
             (body () ,@(read-markdown)))
      display-xexpr))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; The following is just for interactive development:

;; (require racket/runtime-path)

;; (define-runtime-path test.md "test/test.md")
;; (define xs (parameterize ([current-allow-html? #t])
;;              (with-input-from-file test.md read-markdown)))

;; (define-runtime-path test.css "test/test.css")
;; (define style `(link ([href ,(path->string test.css)]
;;                       [rel "stylesheet"]
;;                       [type "text/css"])))

;; (with-output-to-file "/tmp/markdown.html"
;;   #:exists 'replace
;;   (lambda ()
;;     (~> `(html (head () ,style)
;;                (body () ,@xs))
;;         display-xexpr)))

;; (~> `(html (head () ,style)
;;            (body () ,@xs))
;;     display-xexpr)

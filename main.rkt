#lang rackjure

(module+ test
  (require rackunit))

(define (read-markdown)
  (~> (read-blocks)
      filter-br-before-blocks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; block level

(define (read-blocks)
  (let loop ([xs '()])
    (define x (block-level))
    (cond [x (loop (append xs x))]
          [else xs])))

(define (filter-br-before-blocks xs)
  (for/list ([this (in-list xs)]
             [next (in-list (append (drop xs 1) (list "")))]) ;bit slow way
    (match this
      [(list 'br) (match next
                    [(list (or 'blockquote 'pre) _ ...) ""]
                    [else this])]
      [else this])))

(define (block-level) ;; -> or/c #f list?
  (or (heading)
      (code-block-indent)
      (code-block-backtick)
      (blockquote)
      ;; (block-html)
      (other)))

(define (heading) ;; -> or/c #f list?
  (match (try #px"^(#+) ([^\n]+)\n\n")
    [(list _ pounds text)
     (define tag (~> (str "h" (string-length pounds))
                     string->symbol))
     `((,tag ,@(~> text intra-block)))]
    [else #f]))
              
(define (code-block-indent) ;; -> or/c #f list?
  (match (try #px"^(    [^\n]*\n)+\n")
    [(list code _)
     `((pre (code ,(~> code
                       (nuke-all #px"^    ")
                       (nuke-all #px"\n    " "\n")
                       (nuke-all #px"\n+$")))))]
    [else #f]))

(define (code-block-backtick) ;; -> or/c #f list?
  (match (try #px"^```(.*?)\n(.*?\n)```\n")
    [(list _ lang code)
     `((pre (code ([class ,lang])
                  ,(~> code
                       (nuke-all #px"\n+$")))))]
    [else #f]))

(define (blockquote) ;; -> or/c #f list?
  (match (try #px"^> (.+?\n\n)+?")
    [(list _ bq)
     `((blockquote (p ,@(~> bq 
                            (nuke-all #px"\n+$" "")
                            (nuke-all #px"\n> " " ")
                            (nuke-all #px"\n" " ")
                            intra-block))))]
    [else #f]))

(define (block-html) ;; -> or/c #f list?
  (match (try #px"^<(.+?)( .+)?>(.*)</.+?>")
    [(list _ tag attribs body)
     (displayln (str #:sep " / "
                     "block literal html:" tag attribs body))
     `((,(string->symbol tag)
        (,@(replace (if attribs (list attribs) (list))
                    #px"\\s*(.+?)\\s*=\\s*['\"](.+?)['\"]\\s*"
                    (lambda (_ k v)
                      (list (string->symbol k) v))))
       ,(with-input-from-string body block-html)))]
    [else #f]))

(define (other) ;; -> or/c #f list?
  (match (try #px"^(.+?)\n\n")
    [(list _ text)
     `((p ,@(intra-block text)))]
    [else
     (let ([s (read-line (current-input-port) 'any)])
       (and (not (eof-object? s))
            `(,@(intra-block (str s "\n")))))]))
  ;; (let ([s (read-line (current-input-port) 'any)])
  ;;   (and (not (eof-object? s))
  ;;        `(,@(intra-block (str s "\n"))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; intra-block

(define (intra-block s) ;; s -> (listof xexpr?)
  ;; Look for formatting within a block
  (~> s
      list
      hr
      space&space&newline->br
      remove-newlines
      code
      image
      link
      bold
      italic
      intra-block-html
      filter-br-before-blocks
      ))

(module+ test
  (check-equal? (intra-block "The expression `2 * foo_bar`")
                '("The expression " (code "2 * foo_bar"))))

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

(define (intra-block-html xs) ;; only for intra-block; no nested tags
  (replace xs #px"<(.+?)( .+?)?>(.*?)</.+?>"
           (lambda (_ tag attribs body)
             (displayln (str #:sep " / "
                             "intra literal html" tag attribs body))
             `(,(string->symbol tag)
               (,@(replace (if attribs (list attribs) (list))
                    #px"\\s*(.+?)\\s*=\\s*['\"](.+?)['\"]\\s*"
                    (lambda (_ k v)
                      (list (string->symbol k) v))))
               ,body))))

(define (space&space&newline->br xs)
  (replace xs #px"  \n" (lambda (_) `(br))))

(define (remove-newlines xs)
  (replace xs #px"\n" (lambda (_) " ")))

(define (hr xs)
  (replace xs #px"(?:[*-] ?){3,}" (lambda (_) `(hr))))

(define (image xs)
  (replace xs #px"!\\[(.*?)\\]\\(([^ ]+)(\\s+\"(.+?)\"\\s*)?\\)"
           (lambda (_ alt src __ title)
             `(img ([alt ,alt][src ,src][title ,title])))))

(define (link xs)
  (replace xs #px"\\[(.*?)\\]\\((.+?)\\)"
           (lambda (_ text href)
             `(a ([href ,href]) ,text))))

(define (code xs)
  (define (code-xexpr _ x) `(code ,x))
  (~> xs
      (replace #px"`` ?(.+?) ?``" code-xexpr)
      (replace #px"`(.+?)`" code-xexpr)))

(module+ test
  ;; See http://daringfireball.net/projects/markdown/syntax#code
(define (auto-link xs)
  (define (a _ uri)
    `(a ([href ,uri]) ,uri))
  (~> xs
      (replace #px"<(http.+?)>" a)
      (replace #px"<(www\\..+?)>" a)
      (replace #px"<(.+\\.(?:com|net|org).*)>" a)))

(module+ test
  (check-equal? (auto-link '("<http://www.google.com/path/to/thing>"))
                '((a ((href "http://www.google.com/path/to/thing")) "http://www.google.com/path/to/thing")))
  (check-equal? (auto-link '("<www.google.com/path/to/thing>"))
                '((a ((href "www.google.com/path/to/thing")) "www.google.com/path/to/thing")))
  (check-equal? (auto-link '("<google.com/path/to/thing>"))
                '((a ((href "google.com/path/to/thing")) "google.com/path/to/thing"))))

  (check-equal? (code '("This is some `inline code` here"))
                '("This is some " (code "inline code") " here"))
  (check-equal? (code '(" `` ` `` "))
                '(" " (code "`") " "))
  (check-equal? (code '(" `` `foo` ``"))
                '(" " (code "`foo`")))
  (check-equal? (code '("``There is a literal backtick (`) here.``"))
                '((code "There is a literal backtick (`) here."))))

;; NOTE: Tricky part here is that `_` is considered a \w word char but
;; `*` is not. Therefore using \b in pregexp works for `_` but not for
;; `*`. Argh.
(define (bold xs)
  (replace xs #px"\\b[_*]{2}(.+?)[_*]{2}\\b"
           (lambda (_ x) `(b ,x))))

(module+ test
  (check-equal? (bold '("no __YES__ no __YES__"))
                '("no " (b "YES") " no " (b "YES")))
  ;; TO-DO: Fix *. See note above re \\b
  ;; (check-equal? (bolds '("no **YES** no **YES**"))
  ;;               '("no " (b "YES") " no " (b "YES")))
  (check-equal? (bold '("2*3*4"))
                '("2*3*4")))

(define (italic xs)
  ;; Complicated by need to handle \_literal underlines\_ and asterisks.
  (define (dethunk xs)
    (for/list ([x xs]) (if (procedure? x) (x) x)))
  (~> xs
      (replace #px"\\\\([_*])(.+?)\\\\([_*])"
               (lambda (_ open x close)
                 (thunk (string-append open x close)))) ;so no match next
      (replace #px"\\b(?<!\\\\)[_*]{1}(.+?)[_*]{1}\\b"
               (lambda (_ x) `(i ,x)))
      dethunk))

(module+ test
  (check-equal? (italic '("no _YES_ no _YES_"))
                '("no " (i "YES") " no " (i "YES")))
  ;; TO-DO: Fix *. See note above re \\b
  ;; (check-equal? (italic '("no *YES* no *YES*"))
  ;;               '("no " (i "YES") " no " (i "YES")))
  (check-equal? (italic '("no_no_no"))
                '("no_no_no"))
  (check-equal? (italic '("_YES_ no no_no _YES_YES_ _YES YES_"))
                '((i "YES") " no no_no " (i "YES_YES") " " (i "YES YES")))
  (check-equal? (italic '("\\_text surrounded by literal underlines\\_"))
                '("_text surrounded by literal underlines_"))
  (check-equal? (italic '("\\*text surrounded by literal asterisks\\*"))
                '("*text surrounded by literal asterisks*")))

(define (try re)
  (define xs (regexp-try-match re (current-input-port)))
  (and xs (map (lambda (x)
                 (and x (bytes->string/utf-8 x)))
               xs)))

(define (nuke-all s re [new ""])
  (regexp-replace* re s new))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/runtime-path)

(define-runtime-path test.md "test.md")
(define sample (with-input-from-file test.md read-markdown))

(pretty-print sample)

(define-runtime-path test.css "test.css")
(define style `(link ([href ,(path->string test.css)]
                      [rel "stylesheet"]
                      [type "text/css"])))

(require xml)
(define html (xexpr->string `(html (head () ,style) (body () ,@sample))))
(with-output-to-file "/tmp/markdown.html" #:exists 'replace
  (lambda () (display html)))

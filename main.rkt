#lang rackjure

(module+ test
  (require rackunit))

(define (read-markdown)
  (~> (read-blocks)
      filter-br-before-blocks))

(define (read-blocks)
  (let loop ([xs '()])
    (define x (block-level))
    (cond [x (loop (append xs x))]
          [else xs])))

(define (filter-br-before-blocks xs)
  (for/list ([this (in-list xs)]
             [next (in-list (append (drop xs 1) (list "")))])
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
      (other)))

(define (heading) ;; -> or/c #f list?
  (match (try #px"^(#+) ([^\n]+)\n\n")
    [(list _ pounds text)
     (define tag (~> (format "h~a" (string-length pounds)) string->symbol))
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

(define (other)
  ;; (match (try #px"^(.+?)\n\n")
  ;;   [(list _ text)
  ;;    `(,@(intra-block text))]
  ;;   [else
  ;;    (let ([s (read-line (current-input-port) 'any)])
  ;;      (and (not (eof-object? s))
  ;;           `(,@(intra-block (string-append s "\n")))))]))
  (let ([s (read-line (current-input-port) 'any)])
    (and (not (eof-object? s))
         `(,@(intra-block (string-append s "\n"))))))

(define (intra-block s) ;; s -> (listof xexpr?)
  ;; Look for formatting within a block
  (~> s
      newlines->brs ;; also converts from string? to listof xexpr?
      filter-empty-strings
      code
      image
      hr
      link
      bold
      italic
      filter-br-before-blocks
      filter-empty-strings
      ))

(define (try re)
  (define xs (regexp-try-match re (current-input-port)))
  (and xs (map bytes->string/utf-8 xs)))

(define (nuke-all s re [new ""])
  (regexp-replace* re s new))

(define (newlines->brs s) ;; string? -> list?
  (~> (regexp-split #px"\n" s)
      (add-between `(br))))

(define (filter-empty-strings xs)
  (filter (negate (curry equal? "")) xs))

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

(define (hr xs)
  (replace xs #px"^(?:(?:[*-][ ]{0,}){3,}\n)"
           (lambda (_)
             `(hr))))

(hr '("---\n\n---\nblah blah\n---\n"))

(define (image xs)
  (replace xs #px"!\\[(.*?)\\]\\(([^ ]+)(\\s+\"(.+?)\"\\s*)?\\)"
           (lambda (_ alt src __ title)
             `(img ([alt ,alt][src ,src][title ,title])))))

(define (link xs)
  (replace xs #px"\\[(.*?)\\]\\((.+?)\\)"
           (lambda (_ text href)
             `(a ([href ,href]) ,text))))

(define (code xs)
  (replace xs #px"`(.+?)`" (lambda (_ x) `(code ,x))))

;; NOTE: Tricky part here is that `_` is considered a \w word char but
;; `*` is not. Therefore using \b in pregexp works for `_` but not for
;; `*`. Argh.
(define (bold xs)
  (replace xs #px"\\b[_*]{2}(.+?)[_*]{2}\\b"
           (lambda (_ x) `(b ,x))))
(define (italic xs)
  (replace xs #px"\\b[_*]{1}(.+?)[_*]{1}\\b"
           (lambda (_ x) `(i ,x))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)

  (check-equal? (code '("This is some `inline code` here"))
                '("This is some " (code "inline code") " here"))

  (check-equal? (italic '("no _YES_ no _YES_"))
                '("no " (i "YES") " no " (i "YES")))
  ;; TO-DO: Fix *
  ;; (check-equal? (italic '("no *YES* no *YES*"))
  ;;               '("no " (i "YES") " no " (i "YES")))
  (check-equal? (italic '("no_no_no"))
                '("no_no_no"))
  (check-equal? (italic '("_YES_ no no_no _YES_YES_ _YES YES_"))
                '((i "YES") " no no_no " (i "YES_YES") " " (i "YES YES")))

  (check-equal? (bold '("no __YES__ no __YES__"))
                '("no " (b "YES") " no " (b "YES")))
  ;; TO-DO: Fix *
  ;; (check-equal? (bolds '("no **YES** no **YES**"))
  ;;               '("no " (b "YES") " no " (b "YES")))
  (check-equal? (bold '("2*3*4"))
                '("2*3*4"))

  (check-equal? (intra-block "The expression `2 * foo_bar`")
                '("The expression " (code "2 * foo_bar")))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/runtime-path)

(define-runtime-path test.md "test.md")
(define sample (with-input-from-file test.md read-markdown))

;; (pretty-print sample)

(define-runtime-path test.css "test.css")
(define style `(link ([href ,(path->string test.css)]
                      [rel "stylesheet"]
                      [type "text/css"])))

(require xml)
(define html (xexpr->string `(html (head () ,style) (body () ,@sample))))
(with-output-to-file "/tmp/markdown.html" #:exists 'replace
  (lambda () (display html)))

#lang rackjure

(provide xexprs->scribble-pres)

(require scribble/base
         (prefix-in core: scribble/core)
         scribble/html-properties
         scribble/manual
         scribble/decode
         (only-in xml xexpr?))

;; Given a list of xexprs representing valid HTML, return a Scribble
;; representation: A list of pre-part? pre-flow? or pre-content?
;; acceptable to Scribble's `decode`.
;;
;; Although this could be generalized, currently it's only intended to
;; handle the subset of HTML that read-markdown returns.
(define/contract (xexprs->scribble-pres xs)
  ((listof xexpr?) . -> . (listof (or/c pre-part? pre-flow? pre-content?)))
  (xs->ps xs))

(define (xs->ps xs)
  (for/list ([x (in-list xs)])
    (match x
      [`(,(and sec (or 'h1 'h2 'h3)) ((id ,name) [,_ ,_]...) ,es ...)
       (define mk (case sec
                    [(h1) section]
                    [(h2) subsection]
                    [(h3) subsubsection]))
       (mk #:tag name (xs->ps es))]
      [(or `(table ([,_ ,_] ...) (tbody ([,_ ,_] ...) ,rows ...))
           `(table ([,_ ,_] ...) ,rows ...))
       (tabular #:sep (hspace 1)
                #:style 'boxed
                (for/list ([row rows])
                  (match row
                    [`(tr ([,_ ,_] ...) ,cells ...)
                     (for/list ([cell cells])
                       (match cell
                         [`(td ([,_ ,_] ...) ,es ...)
                          (xs->ps es)]
                         [_ ""]))])))]
      [`(p () ,es ...) (para (xs->ps es))]
      [`(pre ([class "brush: racket"]) (code () ,s)) (codeblock "#lang racket\n" s)]
      [`(pre () (code () ,s)) (codeblock s)]
      [`(blockquote () ,es ...) (centered (xs->ps es))]
      [`(ul () ,es ...) (itemlist (xs->ps es))]
      [`(ol () ,es ...) (itemlist #:style 'ordered (xs->ps es))]
      [`(li () ,es ...) (item (xs->ps es))]
      [`(em () ,es ...) (italic (xs->ps es))]
      [`(strong () ,es ...) (bold (xs->ps es))]
      [`(code () ,es ...) (tt (xs->ps es))]
      [`(br ()) (linebreak)]
      [`(span ([,_ ,_] ...) ,es ...) (xs->ps es)]
      [`(sup () ,es ...) (superscript (xs->ps es))]
      [`(sub () ,es ...) (subscript (xs->ps es))]
      [`(hr ()) (apply centered (for/list ([_ 20]) 'mdash))] ;;better way?
      [`(a ,(list-no-order `[href ,href] `[name ,name]) ,es ...)
       (hyperlink #:style (core:style "anchor" (list (url-anchor name)))
                  href (xs->ps es))]
      [`(a ,(list-no-order `[href ,href]) ,es ...)
       (hyperlink href (xs->ps es))]
      [`(a ([name ,name]))
       (elem #:style (core:style "anchor" (list (url-anchor name))) "")]
      [`(div ([class "figure"])
         (img ,(list-no-order `[src ,src] `[alt ,alt] `[title ,title]))
         (p ([class "caption"]) ,alt2))
       "image: TO-DO"] ;; Can Scribble inline an external image ??
      [`(img ,(list-no-order `[src ,src] `[alt ,alt] `[title ,title]))
       "image: TO-DO"] ;; Can Scribble inline an external image ??
      [(? string? s) s]
      [(? symbol? s) s]
      ;; TO-DO: Footnotes
      [x (format "IGNORING ~v\n" x)])))

(module+ test
  (require rackunit)
  (define-syntax (chk stx)
    (syntax-case stx ()
      [(_ input expected)
       (syntax/loc stx
         (check-equal? (xs->ps (list input))
                       (list expected)))]))
  (chk '(h1 ([id "name"]) "h1") (section #:tag "name" (list "h1")))
  (chk '(h2 ([id "name"]) "h2") (subsection #:tag "name" (list "h2")))
  (chk '(h3 ([id "name"]) "h3") (subsubsection #:tag "name" (list "h3")))
  (chk '(p () "hi" "there") (para "hi" "there"))
  (chk '(div
         ((class "figure"))
         (img
          ((src "http://racket-lang.org/logo.png")
           (alt "alt")
           (title "Racket logo")))
         (p ((class "caption")) "alt"))
       "image: TO-DO")
  #;
  (chk '(table ()
         (tbody ()
                (tr () (td () "hi"))))
       (tabular #:sep (hspace 1)
                #:style 'boxed
                (list (list "hi")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Example usage

;; (require scribble/base-render
;;          (prefix-in html: scribble/html-render)
;;          racket/runtime-path
;;          markdown)

;; (define work-dir (find-system-path 'temp-dir))

;; (define (build-html-doc docs dest-file)
;;   (let* ([renderer (new (html:render-mixin render%) [dest-dir work-dir])]
;;          [fns      (list (build-path work-dir dest-file))]
;;          [fp       (send renderer traverse docs fns)]
;;          [info     (send renderer collect  docs fns fp)]
;;          [r-info   (send renderer resolve  docs fns info)])
;;     (send renderer render docs fns r-info)
;;     (send renderer get-undefined r-info)))

;; (define-runtime-path test.md "test/test.md")
;; (define part (~> (with-input-from-file test.md read-markdown)
;;                  ((lambda (v) (pretty-print v) v))
;;                  xexprs->scribble-pres
;;                  decode))
;; (build-html-doc (list part) "test.html")
;; (require net/sendurl)
;; (send-url (str "file://" work-dir "test.html"))

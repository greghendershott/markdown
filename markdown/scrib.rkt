#lang racket/base

(require (for-syntax racket/base)
         racket/contract
         racket/match
         (prefix-in core: scribble/core)
         scribble/html-properties
         scribble/manual
         scribble/decode
         (only-in xml xexpr?))

(provide
 (contract-out
  [rename xs->ps xexprs->scribble-pres
          (-> (listof xexpr?)
              (listof (or/c pre-part? pre-flow? pre-content?)))]))

;; Given a list of xexprs representing valid HTML, return a Scribble
;; representation: A list of pre-part? pre-flow? or pre-content?
;; acceptable to Scribble's `decode`.
;;
;; Although this could be generalized, currently it's only intended to
;; handle the subset of HTML that read-markdown returns.
(define (xs->ps xs [proc (λ _ #f)])
  (for/list ([x (in-list xs)])
    (match x
      [(app proc (and v (not #f))) v]
      [`(,(and sec (or 'h1 'h2 'h3)) ((id ,name) [,_ ,_]...) . ,es)
       (define mk (case sec
                    [(h1) section]
                    [(h2) subsection]
                    [(h3) subsubsection]))
       (mk #:tag name (xs->ps es))]
      [(or `(table ([,_ ,_] ...) (tbody ([,_ ,_] ...) . ,rows))
           `(table ([,_ ,_] ...) ,rows ...))
       (tabular #:sep (hspace 1)
                #:style 'boxed
                (for/list ([row rows])
                  (match row
                    [`(tr ([,_ ,_] ...) . ,cells)
                     (for/list ([cell cells])
                       (match cell
                         [`(td ([,_ ,_] ...) . ,es)
                          (xs->ps es)]
                         [_ ""]))])))]
      [`(p () . ,es) (para (xs->ps es))]
      [`(pre ([class "brush: racket"]) (code () ,s)) (codeblock "#lang racket\n" s)]
      [`(pre () (code () ,s)) (verbatim s)]
      [`(blockquote () . ,es) (nested #:style 'inset (xs->ps es))]
      [`(ul () . ,es) (itemlist (xs->ps es))]
      [`(ol () . ,es) (itemlist #:style 'ordered (xs->ps es))]
      [`(li () . ,es) (item (xs->ps es))]
      [`(em () . ,es) (italic (xs->ps es))]
      [`(strong () . ,es) (bold (xs->ps es))]
      [`(code () . ,es) (tt (xs->ps es))]
      [`(br ()) (linebreak)]
      [`(span ([,_ ,_] ...) . ,es) (xs->ps es)]
      [`(sup () . ,es) (superscript (xs->ps es))]
      [`(sub () . ,es) (subscript (xs->ps es))]
      [`(hr ()) (apply centered (for/list ([_ 20]) 'mdash))] ;;better way?
      [`(a ,(list-no-order `[href ,href] `[name ,name]) . ,es)
       (hyperlink #:style (core:style "anchor" (list (url-anchor name)))
                  href (xs->ps es))]
      [`(a ,(list-no-order `[href ,href]) . ,es)
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

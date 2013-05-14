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
  (for/list ([x xs])
    (match x
      [`(h1 (a ([name ,name][,_ ,_]...)) ,es ...)
       (section #:tag name (xs->ps es))]
      [`(h2 (a ([name ,name][,_ ,_]...)) ,es ...)
       (subsection #:tag name (xs->ps es))]
      [`(h3 (a ([name ,name][,_ ,_]...)) ,es ...)
       (subsubsection #:tag name (xs->ps es))]
      [`(p (table ([,_ ,_] ...) ...
                  (tbody ([,_ _] ...) ...
                         ,rows ...)))
       (tabular #:sep (hspace 1)
                #:style 'boxed
                (for/list ([row rows])
                  (match row
                    [`(tr ([,_ ,_] ...) ... ,cells ...)
                     (for/list ([cell cells])
                       (match cell
                         [`(td ([,_ ,_] ...) ... ,es ...)
                          (xs->ps es)]
                         [else ""]))])))]
      [`(p ,es ...) (para (xs->ps es))]
      [`(pre ([class "brush: racket"]) ,s) (racketblock s)]
      [`(pre ,s) (codeblock s)]
      [`(blockquote ,es ...) (centered (xs->ps es))]
      [`(ul ,es ...) (itemlist (xs->ps es))]
      [`(ol ,es ...) (itemlist #:style 'ordered (xs->ps es))]
      [`(li ,es ...) (item (xs->ps es))]
      [`(em ,s) (italic s)]
      [`(strong ,s) (bold s)]
      [`(code ,s) (tt s)]
      [`(br) (linebreak)]
      [`(span ([,_ ,_] ...) ... ,es ...) (xs->ps es)]
      [`(sup ,es ...) (superscript (xs->ps es))]
      [`(sub ,es ...) (subscript (xs->ps es))]
      [`(hr) (apply centered (for/list ([_ 20]) 'mdash))] ;;better way?
      [`(a ,(list-no-order `[href ,href] `[name ,name]) ,es ...)
       (hyperlink #:style (core:style "anchor" (list (url-anchor name)))
                  href (xs->ps es))]
      [`(a ,(list-no-order `[href ,href]) ,es ...)
       (hyperlink href (xs->ps es))]
      [`(a ([name ,name]))
       (elem #:style (core:style "anchor" (list (url-anchor name))) "")]
      [`(img ,(list-no-order `[src ,src] `[alt ,alt] `[title ,title]))
       "image: TO-DO"] ;; Can Scribble link to an external image ??
      [(? string? s) s]
      [(? symbol? s) s]
      [(var x) (format "IGNORING ~v\n" x)]
      )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Example usage

#|

(require scribble/base-render
         (prefix-in html: scribble/html-render)
         racket/runtime-path
         markdown)

(define work-dir (find-system-path 'temp-dir))

(define (build-html-doc docs dest-file)
  (let* ([renderer (new (html:render-mixin render%) [dest-dir work-dir])]
         [fns      (list (build-path work-dir dest-file))]
         [fp       (send renderer traverse docs fns)]
         [info     (send renderer collect  docs fns fp)]
         [r-info   (send renderer resolve  docs fns info)])
    (send renderer render docs fns r-info)
    (send renderer get-undefined r-info)))

(define-runtime-path test.md "test/test.md")
(define part (~> (with-input-from-file test.md read-markdown)
                 xexprs->scribble-pres
                 decode))
(build-html-doc (list part) "test.html")
(require net/sendurl)
(send-url (str "file://" work-dir "test.html"))

|#

;; Copyright (c) 2013-2022 by Greg Hendershott.
;; SPDX-License-Identifier: BSD-2-Clause

#lang racket/base

(require racket/contract
         racket/function
         racket/list
         racket/format
         racket/match
         racket/port
         (only-in xml/xexpr xexpr?))

(provide
 (contract-out
  [display-xexpr (-> xexpr? any)]
  [xexpr->string (-> xexpr? string?)]))

;; xexpr->string does too little formatting (making diff-ing HTML less
;; friendly) and display-xml does too much. We need a warm bowl of
;; porridge.
;;
;; Furthermore, we should avoid escaping when not required. For
;; example `&` need not be escaped in attribute values, nor in certain
;; "raw text" elements like script and style.

(define (display-xexpr x)
  (display-xexpr* x #:indent 0 #:pre 0 #:raw? #f))

(define (xexpr->string x)
  (with-output-to-string (λ () (display-xexpr x))))

(define (display-xexpr* x #:indent indent #:pre pre #:raw? raw?)
  (match x
    [(? symbol? s) (printf "&~a;" s)]
    [(? integer? n) (printf "&#~a;" n)]
    [(list '!HTML-COMMENT (list) (? string? text)) (printf "<!--~a-->" text)]
    [(list* (? symbol? tag) (list (list (? symbol? ks) vs) ...) contents)
     (when (and (zero? pre)
                (newline-and-indent? tag))
       (newline)
       (display (make-string indent #\space)))
     (display "<")
     (display tag)
     (for ([k (in-list ks)]
           [v (in-list vs)])
       (printf " ~a=\"~a\"" k (escape-attribute v)))
     (if (and (empty? contents) (void-element? tag))
         (display " />")
         (let ([indent (add1 indent)]
               [pre    (if (eq? tag 'pre) (add1 pre) pre)]
               [raw?   (raw-element? tag)])
           (display ">")
           (for ([c contents])
             (display-xexpr* c #:indent indent #:pre pre #:raw? raw?))
           (printf "</~a>" tag)))]
    [(cons (? symbol? tag) contents)
     (display-xexpr* (list* tag '()  contents)
                     #:indent indent #:pre pre #:raw? raw?)]
    [v (display ((if raw? values escape-contents) (~a v)))]))

(define (escape table x)
  (regexp-replace* table x
                   (λ (s)
                     (case (string-ref s 0)
                       [(#\<) "&lt;"]
                       [(#\>) "&gt;"]
                       [(#\&) "&amp;"]
                       [(#\") "&quot;"]))))
(define escape-attribute (curry escape #rx"[<>&\"]"))
(define escape-contents  (curry escape #rx"[<>&]"))

(define (newline-and-indent? tag)
  ;; from https://developer.mozilla.org/en-US/docs/Web/HTML/Inline_elements
  (not (memq tag '(a abbr acronym audio
                     b bdi bdo big br button
                     canvas cite code
                     data datalist del dfn
                     em embed
                     i iframe img input ins
                     kbd
                     label
                     map mark meter
                     noscript
                     object output
                     picture progress
                     q
                     ruby
                     s samp script select slot small span strong sub sup svg
                     template textarea time
                     u
                     tt
                     var video
                     wbr))))

(define (void-element? tag)
  ;; Note: I'm not using Racket xml collection's
  ;; `html-empty-tags`. Instead, using HTML5 list of void elements
  ;; from https://www.w3.org/TR/html5/syntax.html#void-elements
  (boolean
   (memq tag '(area base br col command embed hr img input keygen link
                    meta param source track wbr))))

(define (raw-element? tag)
  ;; HTML5 defines these as "raw text" elements:
  ;; https://www.w3.org/TR/html5/syntax.html#raw-text-elements
  (boolean (memq tag '(script style))))

(define (boolean v)
  (not (not v)))

(module+ test
  (require rackunit)
  (check-equal? (xexpr->string '(a ([href "/path/to?a=1&b=2"]) "AT&T"))
                "<a href=\"/path/to?a=1&amp;b=2\">AT&amp;T</a>")
  ;; Ampersand SHOULD be escaped in <pre>
  (check-equal? (xexpr->string '(pre () "a;\nb;\n1 & 2;\n"))
                "\n<pre>a;\nb;\n1 &amp; 2;\n</pre>")
  ;; Ampersand should NOT be escaped in <script> or <style>, because
  ;; HTML5 defines these as raw text elements.
  (check-equal? (xexpr->string '(script () "a;\nb;\n1 & 2;\n"))
                "<script>a;\nb;\n1 & 2;\n</script>")
  (check-equal? (xexpr->string '(style () "a;\nb;\n1 & 2;\n"))
                "\n<style>a;\nb;\n1 & 2;\n</style>"))

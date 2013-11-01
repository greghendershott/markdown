#lang racket

(require rackjure/threading)

(provide display-xexpr)

;; xexpr->string does too little formatting, and display-xml does too
;; much.  This is the warm bowl of porridge.

(define current-pre (make-parameter 0))

(define (display-xexpr x [indent 0])
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
  (define (f tag ks vs body)
    (when (eq? tag 'pre)
      (current-pre (add1 (current-pre))))
    (define-values (newline-str indent-str)
      (cond [(> (current-pre) 1) (values "" "")]
            [(memq tag '(a code em img span strong sup)) (values "" "")]
            [else (values "\n" (make-string indent #\space))]))
    (printf "~a~a<~a" newline-str indent-str tag)
    (for ([k ks]
          [v vs])
      (printf " ~a=\"~a\"" k (escape v escape-attribute-table)))
    (cond [(and (empty? body) (void-element? tag)) (display " />")]
          [else (printf ">")
                (for ([b body])
                  (display-xexpr b (+ 1 indent)))
                (printf "</~a>" tag)])
    (when (eq? tag 'pre)
      (current-pre (sub1 (current-pre)))))
  (match x
    [`(!HTML-COMMENT () ,x) (~> (format "<!--~a-->" x) display)]
    [`(,(? symbol? tag) ([,ks ,vs] ...) ,els ...) (f tag ks vs els)]
    [`(,(? symbol? tag) ,els ...) (f tag '() '() els)]
    [(? symbol? x) (~> (format "&~a;" x) display)]
    [(? integer? x) (~> (format "&#~a;" x) display)]
    [_ (~> x ~a (escape escape-table) display)]))

(define (void-element? x)
  ;; Note: I'm not using Racket xml collection's
  ;; `html-empty-tags`. Instead, using HTML5 list of void elements
  ;; from http://www.w3.org/TR/html5/syntax.html#void-elements
  (memq x '(area base br col command embed hr img input keygen link
                 meta param source track wbr)))

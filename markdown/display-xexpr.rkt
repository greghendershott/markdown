#lang racket/base

(require racket/contract
         racket/list
         racket/format
         racket/function
         racket/match
         racket/port
         rackjure/threading
         (only-in xml/xexpr xexpr?))

(provide
 (contract-out [display-xexpr (->* (xexpr?) (0) any)]
               [xexpr->string (-> xexpr? string?)]))

;; xexpr->string does too little formatting, and display-xml does too
;; much. This is the warm bowl of porridge.

(define current-pre (make-parameter 0))
(define current-script (make-parameter #f))

(define (display-xexpr x [indent 0])
  (define escape-entity-table    #rx"[<>&]")
  (define escape-attribute-table #rx"[<>&\"]")
  (define escape-script-table #rx"[\"]")
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
    (when (eq? tag 'script)
      (current-script #t))
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
      (current-pre (sub1 (current-pre))))
    (when (eq? tag 'script)
      (current-script #f)))
  (match x
    [`(!HTML-COMMENT () ,x) (~> (format "<!--~a-->" x) display)]
    [`(,(? symbol? tag) ([,ks ,vs] ...) . ,els) (f tag ks vs els)]
    [`(,(? symbol? tag) . ,els) (f tag '() '() els)]
    [(? symbol? x) (~> (format "&~a;" x) display)]
    [(? integer? x) (~> (format "&#~a;" x) display)]
    [_ (~> x ~a (escape (if (current-script) escape-script-table escape-entity-table)) display)]))

(define (void-element? x)
  ;; Note: I'm not using Racket xml collection's
  ;; `html-empty-tags`. Instead, using HTML5 list of void elements
  ;; from http://www.w3.org/TR/html5/syntax.html#void-elements
  (memq x '(area base br col command embed hr img input keygen link
                 meta param source track wbr)))

;; Unlike Racket's xexpr->string, this does not over-aggressively
;; encode chars like & in attribute values.
(define/contract (xexpr->string x)
  (-> xexpr? string?)
  (with-output-to-string (thunk (display-xexpr x))))

(module+ test
  (require rackunit)
  (check-equal? (xexpr->string '(a ([href "/path/to?a=1&b=2"]) "AT&T"))
                "<a href=\"/path/to?a=1&amp;b=2\">AT&amp;T</a>"))

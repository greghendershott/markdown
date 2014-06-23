#lang at-exp racket

(require (only-in srfi/1 span)
         rackjure/threading
         rackjure/str
         "parse.rkt")

(provide toc)

(define (toc xs) ;; (listof xexpr?) -> xexpr?
  (define (do-list xs) ;; (listof head?) -> (listof xexpr?)
    (let loop ([xs xs])
      (match xs
        ['() '()]
        [(cons x more)
         ;; Get the span of `more` that's subitems, and the remainder
         (define (sub? a b) ;; is b's level > a -- e.g. h2 is sub of h1
           (< (head-level a) (head-level b)))
         (define-values (subs peers) (span (curry sub? x) more))
         ;; Make an xexpr (possibly empty) for the sublists (if any)
         (define (sub-xpr subs)
           (match subs
             ['() '()]
             [_ `((ul ,@(do-list subs)))]))
         ;; Make the `li` xexpr for this and any sublists
         (match-define (head level anchor body) x)
         (define li `(li (a ([href ,anchor]) ,@body)
                         ,@(sub-xpr subs)))
         (cons li (loop peers))])))
  (struct head (level anchor body))
  (define (match-head x) ;; xexpr -> (or/c head? #f)
    (match x
      [`(,(and tag (or 'h1 'h2 'h3)) ;just first few levels
         ([id ,anchor])
         . ,body)
       (define level (~> tag symbol->string (substring 1) string->number))
       (head level (str "#" anchor) body)]
      [_ #f]))

  `(div ([class "toc"])
        (ol ,@(do-list (filter-map match-head xs)))))

(module+ test
  (require rackunit)
  (check-equal?
   (toc (parse-markdown @~a{# 1
                            
                            ## 1.1
                            
                            # 2
                            
                            ## 2.1
                            
                            }))
   '(div ((class "toc"))
         (ol
          (li (a ((href "#1")) "1")
              (ul (li (a ((href "#11")) "1.1"))))
          (li (a ((href "#2")) "2")
              (ul (li (a ((href "#21")) "2.1"))))))))

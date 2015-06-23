#lang racket/base

(require (for-syntax racket/base)
         racket/format
         racket/function
         racket/list
         racket/match
         (only-in parsack
                  [parser-compose pdo] ;; More concise, less indent
                  [parser-one pdo-one] ;; "
                  [parser-seq pdo-seq] ;; "
                  >>= >>
                  try <?> choice $err err
                  satisfy char string stringAnyCase
                  many many1
                  manyTill many1Till
                  manyUntil many1Until
                  sepBy
                  oneOf noneOf oneOfStrings
                  option optional
                  return
                  between
                  lookAhead
                  notFollowedBy
                  $space $newline $anyChar $letter $digit $hexDigit
                  $alphaNum $eof
                  getState setState withState
                  State State? Consumed Consumed! Empty Ok Error Msg
                  parse parse-result parsack-error parse-source
                  incr-pos))

(provide pdo
         pdo-one
         pdo-seq
         >>= >>
         try <?> choice $err err
         satisfy char string stringAnyCase
         many many1
         manyTill many1Till
         manyUntil many1Until
         sepBy
         oneOf noneOf oneOfStrings
         option optional
         return
         between
         lookAhead
         notFollowedBy
         $space $newline $anyChar $letter $digit $hexDigit
         $alphaNum $eof
         getState setState withState
         State State? Consumed Consumed! Empty Ok Error Msg
         parse parse-result parsack-error parse-source
         incr-pos)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require (prefix-in parsack: (only-in parsack <or>))
         (for-syntax syntax/parse racket/syntax))

(provide <or>
         OR-DEBUG:PRINT-RESULTS)

;; toggle OR-DEBUG to turn <or> instrumentation on/off
(define-syntax OR-DEBUG #f)

;; OR-HASH:
;; [MutHashof OR-ID [Hashof OR-BRANCH-ID [Pairof Count Count]]]
;;
;; - OR-ID is some unique identification for each syntactic <or> expression
;; - each OR-ID maps to another hash table that counts how often each branch
;;   parses successfully or not
(define OR-HASH (make-hash))

(struct uses (consumed empty)
        #:methods gen:custom-write
        [(define (write-proc v port mode)
           (define fmt (curry ~a #:width 7 #:align 'right))
           (fprintf port (fmt (uses-consumed v)))
           (fprintf port (fmt (uses-empty v))))])

(define (add1-consumed u)
  (match u [(uses consumed empty) (uses (add1 consumed) empty)]))

(define (add1-empty u)
  (match u [(uses consumed empty) (uses consumed (add1 empty))]))

(define-syntax (instrumented-<or> stx)
  (syntax-parse stx
    [(_ p ...)
     #:with KEY (string->symbol (format "; ~a:~a:~a"
                                        (syntax-source stx)
                                        (syntax-line stx)
                                        (syntax-column stx)))
     #'(parsack:<or>
        (λ (state)
          ;; make sure each branch has some entry
          (hash-update! OR-HASH 'KEY
                        (λ (h) (if (hash-has-key? h 'p)
                                   h
                                   (hash-set h 'p (uses 0 0))))
                        (hash))
          (define res (p state))
          (define (update what)
            (hash-update! OR-HASH 'KEY
                          (λ (h) (hash-update h 'p what (uses 0 0)))
                          (hash)))
          (match res
            [(Empty _) (update add1-empty) res]
            [consumed  (update add1-consumed) consumed]))
        ... )]))

(define-syntax (OR-DEBUG:PRINT-RESULTS stx)
  (if (syntax-local-value #'OR-DEBUG)
      #`(let ()
          (newline)
          (define grand-consumed-count 0)
          (define grand-empty-count 0)
          (for ([(k h) (in-hash OR-HASH)])
            (displayln k)
            (define consumed-count 0)
            (define empty-count 0)
            (for ([k+v (sort (hash->list h)
                             > #:key (compose uses-consumed cdr))])
              (match-define (uses consumed empty) (cdr k+v))
              (set! empty-count (+ empty-count empty))
              (set! consumed-count (+ consumed-count consumed))
              (set! grand-empty-count (+ grand-empty-count empty))
              (set! grand-consumed-count (+ grand-consumed-count consumed))
              (printf "~a: ~a\n" (cdr k+v) (car k+v)))
            (displayln (make-string 40 #\-))
            (printf "~a: Total ~a\n"
                    (uses consumed-count empty-count)
                    (+ consumed-count empty-count))
            (newline))
          (displayln (make-string 40 #\=))
          (printf "~a: Grand Total ~a\n"
                  (uses grand-consumed-count grand-empty-count)
                  (+ grand-consumed-count grand-empty-count)))
      #'(void)))

(define-syntax (<or> stx)
  (syntax-parse stx
    [(_ p ...)
     (if (syntax-local-value #'OR-DEBUG)
         (syntax/loc stx (instrumented-<or> p ...))
         (syntax/loc stx (parsack:<or> p ...)))]))

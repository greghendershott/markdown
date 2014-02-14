#lang typed/racket

(define-type Consumption (U Consumed Empty))
(define-type Parser (State -> Consumption))

(provide Consumption Parser)

(require/typed/provide
 parsack
 [#:struct Pos ([line : Exact-Positive-Integer]
                [col : Exact-Positive-Integer]
                [ofs : Exact-Positive-Integer])]
 [#:struct State ([str : String]
                  [pos : Pos]
                  [user : (HashTable Symbol Any)])]
 ;; State?
 [#:struct Msg ([pos : Pos]
                [str : String]
                [strs : (Listof String)])]
 ;; Consumed!
 [#:struct Ok ([parsed : String]
               [rest : String]
               [msg : String])]
 [#:struct Error ([msg : String])]
 [#:struct Consumed ([reply : (U Ok Error)])]
 [#:struct Empty ([reply : (U Ok Error)])]
 [>>= (Parser (Any -> Parser) -> Parser)]
 [>> (Parser Parser -> Parser)]
 [try (Parser -> Parser)]
 [<or> (Parser * -> Parser)]
 [<?> (Parser String -> Parser)]
 [choice ((Listof Parser) -> Parser)]
 [$err (Parser -> Parser)]
 [satisfy ((Char -> Boolean) -> Parser)]
 [char (Char -> Parser)]
 [string (String -> Parser)]
 [stringAnyCase (String -> Parser)]
 [many (Parser -> Parser)]
 [manyTill (Parser Parser -> Parser)]
 [many1 (Parser -> Parser)]
 [many1Till (Parser Parser -> Parser)]
 [sepBy (Parser Parser -> Parser)]
 [oneOf (String -> Parser)]
 [noneOf (String -> Parser)]
 [oneOfStrings (String * -> Parser)]
 [option (Any Parser -> Parser)]
 [optionMaybe (Parser -> Parser)]
 [optional (Parser -> Parser)]
 [return (Any -> Parser)]
 [between (Parser Parser Parser -> Parser)]
 [lookAhead (Parser -> Parser)]
 [notFollowedBy (Parser -> Parser)]
 [$space Parser]
 [$newline Parser]
 [$anyChar Parser]
 [$letter Parser]
 [$digit Parser]
 [$hexDigit Parser]
 [$alphaNum Parser]
 [$eof Parser]
 [getState (Symbol -> Parser)]
 [setState (Symbol Any -> Parser)]
 [parse (Parser String -> Consumed)]
 [parse-result (Parser String -> Any)]
 ;; parsack-error
 [parse-source (Parameterof Path-String)]
 [incr-pos (Pos Char -> Pos)]
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; syntax
;; (require (only-in parsack
;;                   [parser-compose pdo] ;; More concise, less indent
;;                   [parser-one pdo-one] ;; "
;;                   [parser-seq pdo-seq] ;; "
;;                   Consumed!
;;                   withState))
;; (provide pdo pdo-one pdo-seq Consumed! withState)

;;; HACK: As written in Parsack, these macros expand to the untyped >>=
;;;
;;; Should rewrite them to expand using the >>= from the lexical
;;; environment of the macro invocation. But for now, copy/pasta.

(require (for-syntax syntax/parse racket/syntax))

(provide (rename-out [parser-compose pdo]
                     [parser-seq pdo-seq]
                     [parser-one pdo-one]))

(define-syntax (parser-compose stx)
  (syntax-case stx (<- :)
    [(_ p) #'p]
    [(_ (x : t <- p) e ...)
     #'(>>= p (cast (lambda: ([x : t]) (parser-compose e ...))
                    (Any -> Parser)))]
    [(_ q e ...) #'(>>= q (lambda (_) (parser-compose e ...)))]))

(define-syntax (parser-seq stx)
  (define (add-bind stx)
    (syntax-parse stx #:datum-literals (~)
      [(~ p) #'p]
      [q #`(#,(generate-temporary) <- q)]))
  (syntax-parse stx #:datum-literals (~)
    [(_ p:expr ...
        (~optional (~seq #:combine-with combine:expr) #:defaults ([combine #'list])))
     (with-syntax ([(new-p ...) (map add-bind (syntax->list #'(p ...)))])
       (syntax-parse #'(new-p ...) #:datum-literals (<-)
         [(~and ((~or (x <- q1) q2) ...)
                (q ...))
          ;(printf "~a\n" (syntax->datum #'(q2 ...))) ; uncomment for debugging
          #'(parser-compose q ... (return (combine x ...)))]))]))

(define-syntax-rule (parser-cons x y) (parser-seq x y #:combine-with cons))
(define-syntax (parser-one stx)
  (define (add-bind stx)
    (syntax-parse stx #:datum-literals (~>)
      [(~> p) #'p]
      [q #`(~ q)]))
  (syntax-parse stx #:datum-literals (~>)
    [(_ (~and (~seq (~or (~once (~> q1:expr) 
                                #:name "return parse (wrapped with ~>)"
                                #:too-many "too many parses to return (wrapped with ~>)"
                                #:too-few "missing return parse (wrapped with ~>)") 
                         (~not (~> q2:expr))) ...)
              (~seq p:expr ...)))
     (with-syntax ([(new-p ...) (map add-bind (syntax->list #'(p ...)))])
       #'(parser-seq new-p ... #:combine-with (λ (x) x)))]))

(module+ test
  (require typed/rackunit)
  (check-equal? (parse-result (>> $anyChar
                                  (return "yo"))
                              "a")
                "yo")
  (check-equal? (parse-result (>>= $anyChar
                                   (lambda (x) (return x)))
                              "a")
                #\a)
  (check-equal? (parse-result (parser-compose (x : Char <- $anyChar)
                                              (return x))
                              "a")
                #\a)
  (check-equal? (parse-result (parser-compose (cs : (Listof Char) <- (many $anyChar))
                                              (return cs))
                              "abc")
                '(#\a #\b #\c))
  (check-equal? (parse-result (parser-compose (cs : (Listof Char) <- (many $anyChar))
                                              (return (list->string cs)))
                              "abc")
                "abc"))

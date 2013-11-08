#lang racket

;; Try parsing random characters to see if parser fails.
;; Even a random series of characters should parse successfully.
;; There's no such thing as a syntax error with Markdown.

(require rackunit
         "main.rkt")

(define (random-char)
  (let loop ()
    (define c (integer->char (random 127)))
    (cond [(or (char-alphabetic? c)
               (char-numeric? c)
               (memq c '(#\< #\> #\[ #\] #\( #\) #\_ #\newline))) c]
          [(loop)])))

(define (random-word)
  (list->string (for/list ([i (in-range (add1 (random 10)))])
                  (random-char))))

(define (random-line)
  (string-join (for/list ([i (in-range (+ 5 (random 15)))])
                 (random-word))
               " "))

(define (random-doc num-lines)
  (string-join (for/list ([n (in-range num-lines)])
                 (random-line))
               "\n\n"))

(define (check-lines lines)
  (define (gc) (for ([i 3]) (collect-garbage)))
  (define doc (random-doc lines))
  
  (display lines) (displayln " line random text doc")
  
  (check-not-exn (thunk (void (parse-markdown doc)))))

(define (random-test)
  ;; Warning: Could take very long time to complete:
  ;;
  ;; Note: Any "unresolved reference" errors are OK. They simply mean
  ;; that e.g. a footnote or reference link wasn't defined, which is
  ;; extremely likely to be the case with random text. :) Would be good
  ;; to add option to suppress these.
  (for ([i 100])
    (check-lines 10))
  (for ([i 10])
    (check-lines 50)))


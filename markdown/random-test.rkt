#lang at-exp racket

;; Try parsing random characters to see if parser fails.
;; Even a random series of characters should parse successfully.
;; There's no such thing as a syntax error with Markdown.

(require "main.rkt")

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
  (define doc (random-doc lines))
  ;;(displayln @~a{@lines line random text doc})
  (with-handlers ([exn:fail?
                   (lambda (x)
                     (displayln (exn-message x))
                     (displayln "For source text")
                     (display doc))])
    ;; suppress "unresolved reference" messages
    (parameterize ([current-error-port (open-output-nowhere)])
      (void (parse-markdown doc)))))

(define (random-test)
  ;; Warning: Takes long to complete.
  (for ([i 1000])
    (check-lines 10))
  (for ([i 100])
    (check-lines 50)))

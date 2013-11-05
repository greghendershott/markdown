#lang racket

(require "main.rkt")

(require racket/runtime-path)
(define-runtime-path test.md (build-path "test" "test.md"))

(let ()
  (define reps 5)
  (define doc (let ([s (file->string test.md)])
                (string-join (for/list ([i reps])
                               s)
                             "\n\n")))
  (printf "Using ~a appended ~a times, which is ~a chars and ~a lines.\n"
          test.md
          reps
          (string-length doc)
          (length (regexp-split "\n" doc)))
  (time (void (parse-markdown doc)))
  (newline))

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

(define (f lines)
  (define (gc) (for ([i 3]) (collect-garbage)))
  (define doc (random-doc lines))
  
  (display lines) (displayln " line random text doc")
  
  (gc)
  (time (void (parse-markdown doc)))
  (newline))

(f  1)
(f 10)
(f 20)
(f 30)
(f 40)
(f 50)

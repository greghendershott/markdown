#lang racket

(require (rename-in "main.rkt"  [read-markdown re:read-markdown])
         (rename-in "parse.rkt" [read-markdown pr:read-markdown]))



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
  (display "regexp: ")
  (time (void (with-input-from-string doc re:read-markdown)))
  (display "parsack: ")
  (time (void (with-input-from-string doc pr:read-markdown))))

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
  (display "regexp: ")
  (time (void (with-input-from-string doc re:read-markdown)))
  
  (gc)
  (display "parsack: ")
  (time (void (with-input-from-string doc pr:read-markdown)))
  (newline))

         ; RE    PR
         ; --  ----
(f  1)   ; 10    20
(f 10)   ; 34   168
(f 20)   ; 68   509
(f 30)   ;126  1213
(f 40)   ;154  1810
(f 50)   ;188  3885

;; The regexp parser appears to be O(n), but the parsack one looks
;; close to O(n^2). Update: Now parsack close to O(n logn).


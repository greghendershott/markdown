#lang racket

(require "parse.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; example

(define input #<<EOF
Heading 1
=========
Heading 2
---------
# Heading 1
## Heading 2
### Heading 3
#### Heading 4
##### Heading 5
###### Heading 6

Some **bold** text and _emph_ text and `inline code`.

    verbatim1
    verbatim2

```
Fenced with no lang.
```

Foo:

```racket
(define (x2 x)
  (* x 2))
```

Line 2. [Racket](http://www.racket-lang.org).

> I am blockquote 1.
> I am blockquote 2.

An ordered, tight list:

1. One
2. Two
3. Three

An ordered, loose list:

1. One

2. Two

3. Three

An unordered, tight list:

- One
- Two
- Three

An unordered, loose list:

- One

- Two

- Three

Nested lists:

1. One

    - 1A

    - 1B

2. Two

    - 2A

    - 2B

[ref1][].

<br /> 1 < 2 [1]. Amazing!

Some `<hr>`s:

---

- - -

--------


***

___


A [ref1][] and a [ref2][ref2].

[ref1]: http://www.google.com
[ref2]: http://www.google.com "foo"

Here's a table:

<table border="1">
<tr>
<td>Row 1 _Col_ 1</td>
<td>Row 1 Col 2</td>
</tr>
<tr>
<td>Row 2 Col 1</td>
<td>Row 2 Col 2</td>
</tr>
</table>

Here is a footnote use[^1]. And another[^2].

[^1]: The first paragraph of the definition.
    
    Paragraph two of the definition.
    
    > A blockquote with
    > multiple lines.
    
        a code block
    
    A final paragraph.

[^2]: Another footnote defn.

The end.

Some hard line breaks...  
...with two spaces...  
...at end of each one.

Here's an appostrophe. 'Single quotes'. "Double quotes". "A really _great_ quote."  But don't convert <!-- more -->.

<!-- more -->

EOF
)

;; (pretty-print (parse-markdown input 'foot))

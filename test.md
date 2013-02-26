# Heading level 1

## Heading level 2

Some normal text.

Here is some word-level formatting. Also, this uses two spaces before
newline to ensure hard line breaks:

_Italic_.  
*Italic*.  
__Bold__.  
**Bold**.  
`I am code`.  

This is \_surrounded by literal underlines\_ and this is \*surrounded
by literal asterisks\*.

A single backtick in a code span: `` ` ``

A backtick-delimited string in a code span: `` `foo` ``

Did `<html>` get escaped properly?

I am _italic phrase on one line_.

I am _italic phrase across two
lines_, did it work?

- Bullet 1
- Bullet 2
- Bullet 3

1. Item 1
2. Item 2
3. Item 3

A code block that was denoted by 4 leading spaces:

    Indented code line 1.
    Indented code line 2. These spaces [    ] should be preserved.
    Indented code line 3. Don't do _italic_ or **bold** in here.

A code block that was denoted by backticks:

```racket
Backticked code line 1.
Backticked code line 2. These spaces [    ] should be preserved.
Backticked code line 3. Don't do _italic_ or **bold** in here.
```
Back to non-code-block. Next, how about a block quote:

> Here is a multi-line block quote started
with a single `>` to open it. Word formatting
like _italic_ and __bold__ _should_ work in this
kind of block.

And:

> Another multi-line block quote, where
> each line starts with its own `>` char.

Here is a link:

[Racket](http://www.racket-lang.org/)

Here an auto-link:

<http://www.racket-lang.org>

Finally, here is an image:

![alt](http://racket-lang.org/logo.png "Racket logo")

Following are <hr> elements:

---

***

- - -

* * *

How about some lists?

- UL 1
- UL 2
  - UL 2a
  - UL 2b
- UL 3

1. OL
2. OL
  a. OL
  b. OL
3. OL

How about literal HTML?

<table>
<tr>
<td>Hi</td>
</tr>
<tr>
<td>there</td>
</tr>
</table>

This would be intra-block <span>body</span> and <span key="val">body</span>.

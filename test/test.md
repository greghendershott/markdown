# Heading level 1

## Heading level 2

Heading level 1
===

Heading level 2
---

Some normal text.

Here is some word-level formatting. Also, this uses two spaces before
newline to ensure hard line breaks:

_Italic_.  
*Italic*.  
__Bold__.  
**Bold**.  
`I am code`.  

I am _italic phrase on one line_.

I am _italic phrase wrapped across two
lines in the Mardkdown source_, did that work?

This is \_surrounded by literal underlines\_ and this is \*surrounded
by literal asterisks\*.

A single backtick in a code span: `` ` ``

A backtick-delimited string in a code span: `` `foo` ``

Did `<html>` get escaped properly?

Bulleted (unordered) list:

- Bullet 1
- Bullet 2
- Bullet 3

Ordered list:

1. Item 1
2. Item 2
3. Item 3

Bulleted list with space between bullets:

- Item 1

- Item 2

- Item 3

Bulleted, with sublists:

- Bullet 1
  - Bullet 1a
    - Bullet 1a1
  - Bullet 1b
- Bullet 2
  - Bullet 2a
  - Bullet 2b

Ordered, with sublists:

1. One
  1. One / One
  2. One / Two
2. Two
  1. Two / One
  2. Two / Two

A code block that was denoted by 4 leading spaces:

    Code line 1.
    Code line 2. These spaces [    ] should be preserved.
    Code line 3. Don't do _italic_ or **bold** in here.
    Code line 4. Stuff like <html> and & should be escaped.

A code block that was denoted by backticks:

```racket
Code line 1.
Code line 2. These spaces [    ] should be preserved.
Code line 3. Don't do _italic_ or **bold** in here.
Code line 4. Stuff like <html> and & should be escaped.
```
Back to non-code-block. Next, how about a block quote:

> Here is a multi-line block quote started
with a single `>` to open it. Word formatting
like _italic_ and __bold__ _should_ work in this
kind of block.

And:

> Another multi-line block quote, where
> each line starts with its own `>` char.

Here is a link: [Racket](http://www.racket-lang.org/)

Here are linkref style links: [Racket][1] and [Google][2].

And here are the footnote refs:

[1]: http://www.racket-lang.org/ "Racket"
[2]: http://www.google.com/ "Google"

Here an auto-link: <http://www.racket-lang.org>

Here an auto-link email: <foo@bar.com>

Finally, here is an image:

![alt](http://racket-lang.org/logo.png "Racket logo")

Following are `<hr>` elements:

---

***

- - -

* * *

How about literal HTML?

Here's a table:

<table border="1">
<tr>
<td>Row 1 Col 1</td>
<td>Row 1 Col 2</td>
</tr>
<tr>
<td>Row 2 Col 1</td>
<td>Row 2 Col 2</td>
</tr>
</table>

Here is `<span style="font-weight:bold">span</span>` -- <span
style="font-weight:bold">span</span> -- in the middle of a sentence.

The end.

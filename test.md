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

Bulleted (unordered) list:

- Bullet 1
- Bullet 2
- Bullet 3

> Note: Sub-lists aren't supported.

Ordered list:

1. Item 1
2. Item 2
3. Item 3

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

This would be intra-block <span>span</span> and <span key="val">span</span>.

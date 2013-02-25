# Heading level 1

## Heading level 2

Some normal text.

Here is some word-level formatting:

_Italic_.
*Italic*.
__Bold__.
**Bold**.
`I am code`.

I am _italic phrase on one line_.

I am _italic phrase across two
lines_, did it work?

A code block that was denoted by 4 leading spaces:

    Indented code line 1.
    Indented code line 2. These spaces [    ] should be preserved.
    Indented code line 3. Don't do _italic_ or **bold** in here.

A code block that was denoted by backticks:

```racket
Indented code line 1.
Indented code line 2. These spaces [    ] should be preserved.
Indented code line 3. Don't do _italic_ or **bold** in here.
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

Finally, here is an image:

![alt](http://racket-lang.org/logo.png "Racket logo")

Following are <hr> elements:

---

***

- - -

* * *

How about literal HTML? <span>Test</span>.



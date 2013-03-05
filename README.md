# Quick start

`read-markdown` converts a [Markdown format][1] file to a `(listof
xexpr?)`.

You can modify that (if you wish), splice it into the `body` element
of an `(html ...)` wrapper, and convert to HTML text.

For instance:

```racket
(require xml markdown)

;; 1. Convert the markdown to an xexpr:
(define xexpr (with-input-from-file "foo.md" read-markdown))

;; 2. Optionally, process the xexpr somehow:
;; ... munge ...

;; 3. Splice it into an HTML `xexpr` and...
;; 4. Convert to HTML text:
(display-xexpr `(html ()
                      (head ())
                      (body () ,@xexpr)))
```

`display-xexpr` is provided as a "warm bowl of porridge" -- in between
`xexpr->string` (which does no formatting and isn't very
`diff`-friendly) and `display-xml` (which does too much and can for
example break `<pre>` formatting).

The parameter `current-allow-html?` controls whether HTML in the
markdown file is allowed to be passed through the HTML output. It
defaults to `#t`, which is reasonable for personal use, and supports
the common use case of tables. However this may be dangerous for use
on a web site. In that case either set `current-allow-html?` to `#f`,
or, if you want to support _some_ HTML like `<table>` but not for
example `<script>`, you can walk the `xexpr` and delete the undesired
elements yourself.

The parameter `current-show-linkrefs-as-footnotes?` controls whether
[reference style link definitions][3] are shown in the text, roughly
like footnotes. The default is `#f`, which is the Markdown convention.


# Design

This converts to an `xexpr`, not all the way to HTML text.

## Pros:

- Possible for library user to post-process the `xexpr` (as a sort of
  "DOM") before converting to HTML text. Additive extensions easy to
  do this way.

- During conversion we know what's already converted because it's a
  `list` not a `string`. For instance `"- - -"` becomes `(hr)`, and
  can be easily be ignored by further regular expression matches.

- Things like `<html>` and `&` are automatically escaped by
  `xexpr->string`.
  - Good for `` `code` `` blocks.
  - Good for "safe" markdown conversion (e.g. on web site, don't want
  to permit end users to enter HTML).

## Con:

- Things like `<html>` and `&` are automatically escaped by
  `xexpr->string`.
  - Bad for supporting literal HTML in the markdown, which is part of
  [Gruber's spec](http://daringfireball.net/projects/markdown/).  Very
  common use case: `<table>`s.  We must detect the HTML textually, and
  recreate as `xexprs`.

# Possible future enhancements

- A function to look for Racket code blocks and eval them in a sandbox
  to do an "interaction" style ala' Scribble (showing the expressions
  interleaved with the results).

- A function to look for Racket code blocks, and lex and fontify them
  in the Racket documentation style. (Meanwhile, you can use something
  like Pygments to do this on the server, or a Javascript solution to
  do so on the client such as [SyntaxHighlighter][2]. For the latter,
  the backtick style of specifying code blocks supports a GitHub style
  of specifying the language, which ends up as `lang` in `<code
  class="brush: lang;">`.

[1]: http://daringfireball.net/projects/markdown/basics "Markdown Format"
[2]: http://alexgorbatchev.com/SyntaxHighlighter/manual/brushes/custom.html "SyntaxHighlighter"
[3]: http://daringfireball.net/projects/markdown/syntax#link "Links"

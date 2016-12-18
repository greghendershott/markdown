[![Build Status](https://travis-ci.org/greghendershott/markdown.png?branch=master)](https://travis-ci.org/greghendershott/markdown)
[![raco pkg install markdown](https://img.shields.io/badge/raco_pkg_install-markdown-aa00ff.svg)](http:pkgs.racket-lang.org/#[markdown])
![MIT License](https://img.shields.io/badge/license-MIT-118811.svg)
[![Documentation](https://img.shields.io/badge/Docs-Documentation-blue.svg)](http://docs.racket-lang.org/markdown/index.html)

# Quick start

## Syntax supported

- John Gruber's original [spec][gruber].

- Footnotes as in [PHP Markdown Extra][] and [Python-Markdown][] .

- Fenced code blocks as on [GitHub][gfm]. The optional language is
returned as `(pre ([class "brush: lang"]) ....)`.  You can extract to
use with a highlighter such as Pygments.

- Smart punctuation (quotes, dashes, ellipses).

## Use at the command line, to generate HTML

You can run this at the command-line: Pipe in markdown and it pipes
out HTML.

```sh
$ racket markdown/main.rkt
I am _emph_ and I am **strong**.
^D
<!DOCTYPE html>
<html>
 <head>
  <meta charset="utf-8" /></head>
 <body>
  <p>I am <em>emph</em> and I am <strong>strong</strong>.</p></body></html>
```

## Use as a library, to generate `xexpr`s and HTML

Use `parse-markdown` to convert a `string?` or `path?` to a `(listof xexpr?)`.

You can modify the `(listof xexpr?)`, splice it into the `body`
element of an `(html ...)` wrapper, and convert to HTML text.

Example:

```racket
#lang racket

(require markdown)

;; 1. Parse string to a list of xexprs
(define xs (parse-markdown "I am _emph_ and I am **strong**."))

(pretty-print xs)
; =>
'((p () "I am " (em () "emph") " and I am " (strong () "strong") "."))

;; 2. Optionally, process the xexprs somehow:
;; ... nom nom nom ...

;; 3. Splice them into an HTML `xexpr` and...
;; 4. Convert to HTML text:
(display-xexpr `(html ()
                      (head ())
                      (body () ,@xs)))
; =>
; <html>
;  <head></head>
;  <body>
;   <p>I am <em>emph</em> and I am <strong>strong</strong>.</p></body></html>
```

`display-xexpr` is provided as a "warm bowl of porridge" -- in between
`xexpr->string` (which does no formatting and isn't very
`diff`-friendly) and `display-xml` (which does too much and can for
example break `<pre>` formatting).

## Use as a library, to generate "pre-Scribble"

The `xexpr`s returned by `read-markdown` can also be fed to the
function `xexprs->scribble-pres`, which returns a Scribble
representation -- a `list` of `pre-part?`, `pre-flow?` or `pre-content?`
items -- acceptable to Scribble's `decode`, which returns a Scribble
`part`. The `part` can in turn be fed to any of the Scribble
renderers: HTML, LaTeX, plain text, or even Markdown (if you'd like to
go around in circles).

```racket
#lang rackjure

(require scribble/base-render
         (prefix-in html: scribble/html-render)
         racket/runtime-path
         markdown
         markdown/scrib) ;for `xexprs->scribble-pres`

(define work-dir (find-system-path 'temp-dir))

(define (build-html-doc docs dest-file)
  (let* ([renderer (new (html:render-mixin render%) [dest-dir work-dir])]
         [fns      (list (build-path work-dir dest-file))]
         [fp       (send renderer traverse docs fns)]
         [info     (send renderer collect  docs fns fp)]
         [r-info   (send renderer resolve  docs fns info)])
    (send renderer render docs fns r-info)
    (send renderer get-undefined r-info)))

(define-runtime-path test.md "test/test.md")
(define part (~> (with-input-from-file test.md read-markdown)
                 xexprs->scribble-pres
                 decode))
(build-html-doc (list part) "test.html")
(require net/sendurl)
(send-url (str "file://" work-dir "test.html"))
```

# Notes

Originally this was implemented using a pile of regular expressions,
somewhat like how [Markdown.pl][gruber] works -- but complicated by
creating `xexpr`s not an HTML `string`. On the bright side, I wrote
nearly a hundred unit tests.

In October 2013 I redesigned it to be a "real" parser with a grammar,
using Stephen Chang's [parsack][], a monadic parser combinator
library. The grammar was developed by looking first at
[peg-markdown][] and later at [pandoc][]. For a few edge cases I found
[Babelmark][] and [Babelmark2][] helpful to compare result with many
other implementations. The many unit tests from the old version also
helped.

[gruber]: http://daringfireball.net/projects/markdown/basics
[PHP Markdown Extra]: http://michelf.ca/projects/php-markdown/extra/#footnotes
[python-markdown]: http://pythonhosted.org/Markdown/extensions/footnotes.html
[gfm]: https://help.github.com/articles/github-flavored-markdown
[parsack]: https://github.com/stchang/parsack
[peg-markdown]: https://github.com/jgm/peg-markdown
[pandoc]: https://github.com/jgm/pandoc
[Babelmark]: http://babelmark.bobtfish.net/
[Babelmark2]: http://johnmacfarlane.net/babelmark2/

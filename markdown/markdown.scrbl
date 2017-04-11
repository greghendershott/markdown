#lang scribble/manual
@require[scribble/eval
         (for-label markdown
                    markdown/scrib
                    racket/base
                    racket/class
                    racket/contract
                    (only-in racket/file
                             file->string)
                    racket/runtime-path
                    (only-in scribble/core
                             part)
                    (prefix-in html: scribble/html-render)
                    scribble/base-render
                    scribble/decode
                    (except-in xml
                               xexpr->string))]


@title[#:tag "top"]{markdown}
@author[@hyperlink["https://github.com/greghendershott"]{Greg Hendershott}]

@section[#:tag "quick-start"]{Quick start}

@subsection{Syntax supported}
@itemlist[
@item{
  John Gruber's original @hyperlink["http://daringfireball.net/projects/markdown/basics"]{spec}.
}
@item{
  Footnotes as in @hyperlink["http://michelf.ca/projects/php-markdown/extra/#footnotes"]{PHP Markdown Extra} and @hyperlink["http://pythonhosted.org/Markdown/extensions/footnotes.html"]{Python-Markdown}.
}
@item{
  Fenced code blocks as on @hyperlink["https://help.github.com/articles/github-flavored-markdown"]{GitHub}. The optional language is
returned as @tt{(pre ([class "brush: lang"]) ....)}.  You can extract to
use with a highlighter such as Pygments.
}
@item{
  Smart punctuation (quotes, dashes, ellipses).
}
]

@subsection{Use at the command line, to generate HTML}

You can run this at the command-line: Pipe in markdown and it pipes
out HTML.

@codeblock|{
$ raco markdown
I am _emph_ and I am **strong**.
^D
<!DOCTYPE html>
<html>
 <head>
  <meta charset="utf-8" /></head>
 <body>
  <p>I am <em>emph</em> and I am <strong>strong</strong>.</p></body></html>
}|


@subsection{Use as a library, to generate xexprs and HTML}

Use @racket[parse-markdown] to convert a @racket[string?] or @racket[path?] to a @racket[(listof xexpr?)].

You can modify the @racket[(listof xexpr?)], splice it into the @tt{body}
element of an @tt{(html ...)} wrapper, and convert to HTML text.

@examples[#:eval (make-base-eval #:pretty-print? #t)
(eval:no-prompt (require markdown))

(code:comment "1. Parse string to a list of xexprs")
(eval:no-prompt (define xs (parse-markdown "I am _emph_ and I am **strong**.")))
(eval:no-prompt xs)

(code:comment "2. Optionally, process the xexprs somehow:")
(code:comment "... nom nom nom ...")

(code:comment "3. Splice them into an HTML `xexpr` and...")
(code:comment "4. Convert to HTML text:")
(eval:no-prompt (display-xexpr `(html ()
                      (head ())
                      (body () ,@xs))))
]

@racket[display-xexpr] is provided as a "warm bowl of porridge" --- in between
@racket[xexpr->string] (which does no formatting and isn't very
@tt{diff}-friendly) and @racket[display-xml] (which does too much and can for
example break @tt{<pre>} formatting).

@subsection{Use as a library, to generate "pre-Scribble"}

The @seclink["top" #:doc '(lib "xml/xml.scrbl")]{xexprs} returned by @racket[read-markdown] can also be fed to the
function @racket[xexprs->scribble-pres], which returns a Scribble
representation --- a @racket[list] of @racket[pre-part?], @racket[pre-flow?] or @racket[pre-content?]
items --- acceptable to Scribble's @racket[decode], which returns a Scribble
@tech[#:doc '(lib "scribblings/scribble/scribble.scrbl")]{part}. The @racket[part] can in turn be fed to any of the Scribble
@seclink["renderer" #:doc '(lib "scribblings/scribble/scribble.scrbl")]{renderers}: HTML, LaTeX, plain text, or even Markdown (if you'd
like to go around in circles).

@(begin #reader scribble/comment-reader
@codeblock|{
#lang rackjure

(require scribble/base-render
         (prefix-in html: scribble/html-render)
         racket/runtime-path
         markdown
         (only-in markdown/scrib xexprs->scribble-pres))

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
}|)

@section[#:tag "notes"]{Notes}

Originally this was implemented using a pile of regular expressions,
somewhat like how @hyperlink["http://daringfireball.net/projects/markdown/basics"]{Markdown.pl} works --- but complicated by
creating @racket[xexpr]s not an HTML string. On the bright side, I wrote
nearly a hundred unit tests.

In October 2013 I redesigned it to be a "real" parser with a grammar,
using Stephen Chang's @racketmodname[parsack], a monadic parser combinator
library. The grammar was developed by looking first at
@hyperlink["https://github.com/jgm/peg-markdown"]{peg-markdown} and later at @hyperlink["https://github.com/jgm/pandoc"]{pandoc}.
For a few edge cases I found @hyperlink["http://babelmark.bobtfish.net/"]{Babelmark} and
@hyperlink["http://johnmacfarlane.net/babelmark2/"]{Babelmark2} helpful to compare result with many
other implementations. The many unit tests from the old version also helped.

@subsection[#:tag "bib"]{Links}
@itemlist[
@item{
  @url{http://daringfireball.net/projects/markdown/basics}
}
@item{
  @url{http://michelf.ca/projects/php-markdown/extra/#footnotes}
}
@item{
  @url{http://pythonhosted.org/Markdown/extensions/footnotes.html}
}
@item{
  @url{https://help.github.com/articles/github-flavored-markdown}
}
@item{
  @url{https://github.com/stchang/parsack}
}
@item{
  @url{https://github.com/jgm/peg-markdown}
}
@item{
  @url{https://github.com/jgm/pandoc}
}
@item{
  @url{http://babelmark.bobtfish.net/}
}
@item{
  @url{http://johnmacfarlane.net/babelmark2/}
}
]

@section[#:tag "api"]{API}

@defmodule[markdown]

The @racketmodname[markdown] module provides all bindings from the
@racketmodname[markdown/display-xexpr],
@racketmodname[markdown/parse],
and @racketmodname[markdown/toc] modules.

@subsection{Parsing Markdown}
@defmodule[markdown/parse]

@defproc[(parse-markdown [input (or/c path? string?)] [footnote-prefix-symbol? symbol? (gensym)]) (listof xexpr?)]{
  Parses an entire markdown file.

  When given a @racket[string?], parses the string as a markdown file.
  When given a @racket[path?], calls @racket[file->string] and parses the result.

  @itemlist[
  @item{
    Deletes all @litchar{\r} (including but not limited to @litchar{\r\n}).
  }
  @item{
    Appends a @litchar{\r\n} to simplify whole-document processing.
  }
  ]
}

@defparam[current-strict-markdown? strict? boolean? #:value #t]{
  Parameter to limit the parser to strict markdown (no customizations).
}

@defproc[(read-markdown [footnote-prefix-symbol? symbol? (gensym)]) (listof xexpr?)]{
  Parses markdown input from @racket[current-input-port].

  @deprecated[@racket[parse-markdown]]{Provided for backward compatibility.}
}


@subsection{Building a Table of Contents}
@defmodule[markdown/toc]

@defproc[(toc [xexprs (listof xexpr?)]) xexpr?]{
  Builds a table of contents from the given markdown expression.

  @examples[#:eval (make-base-eval '(require racket/string markdown))
    (toc (parse-markdown (string-join '("# 1" "## 1.1" "# 2" "## 2.1") "\n\n")))
  ]
}


@subsection{Displaying Parsed Markdown}
@defmodule[markdown/display-xexpr]

@defproc[(display-xexpr [xexpr xexpr?] [indent zero?]) any/c]{
  Prints an HTML representation of @racket[xexpr] to @racket[current-output-port].
}

@defproc[(xexpr->string [xexpr xexpr?]) string?]{
  Unlike Racket's @racket[xexpr->string], this does not over-aggressively
  encode chars like @litchar{&} in attribute values.
}


@subsection{Generating Pre-Scribble}
@defmodule[markdown/scrib]

The bindings documented in this section are @bold{not} provided by the @racketmodname[markdown] module.

@defproc[(xexprs->scribble-pres [xexprs (listof xexpr?)]) (listof (or/c pre-part? pre-flow? pre-content?))]{
  Given a list of xexprs representing valid HTML, return a Scribble
  representation: A list of @racket[pre-part?], @racket[pre-flow?], or @racket[pre-content?]
  acceptable to Scribble's @racket[decode].

  Although this could be generalized, currently it's only intended to
  handle the subset of HTML that @racket[read-markdown] returns.
}


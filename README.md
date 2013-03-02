# Quick start

`read-markdown` converts a
[Markdown format](http://daringfireball.net/projects/markdown/basics)
file to a `(listof xexpr?)`.

You can optionally modify that (e.g. to add your own custom extensions
to Markdown).

Then splice that `xexpr` into the `body` element of an `(html ...)`
wrapper, and convert to HTML text.

For instance:

```racket
(require xml
         markdown)

;; 1. Convert the markdown to an xexpr:
(define xexpr (with-input-from-file "foo.md" read-markdown))

;; 2. Optionally, process the xexpr somehow:
;; ... munge ...

;; 3. Splice it into an HTML `xexpr` and...
;; 4. Convert to HTML text:
(xexpr->string `(html ()
                      (head ())
                      (body () ,@xexpr)))
```


# To-do and known issues

Currently none.


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

The end.

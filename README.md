# Quick start

`read-markdown` converts a
[Markdown](http://daringfireball.net/projects/markdown/basics) format
file to a `(listof xexpr?)`. You can optionally modify that (e.g. to
add your own custom extensions to Markdown). Then put it in an `(html
...)` `xexpr` and convert to HTML text.

```racket
(require xml
         markdown)
;; Convert the markdown to an xexpr:
(define xexpr (with-input-from-file "foo.md" read-markdown))
;; Maybe process xexpr:
;; ...
;; Convert to HTML
(xexpr->string `(html ()
                      (head () #| maybe a 'link to style sheet |#)
                      (body () ,@xexpr)))
```


# TO-DO and Known Issues

- Bug: Although underlines work for `_italic_` and `__bold__`,
  asterisks do not (`*italic*` and `**bold**`). Why: I need to figure
  out a work-around for the fact that `\b` doesn't treat `*` as a `\w`
  character for word boundaries.

- Missing: Only hash style headers are implemented. The variations for
  `<h1>` and `<h2>` aren't yet implemented.

```
This h1 style NOT yet supported
===============================

This h2 style NOT yet supported
-------------------------------

# This style IS (h1)

## And this. (h2)

### And this, and so on. (h3+)
```

Only the hash notation goes beyond <h2>, anyway.

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
  `xexpr->string`. Good for `` `code` `` blocks. Good for "safe"
  markdown conversion (e.g. on web site, don't want to permit end
  users to enter HTML).

## Con:

- Things like `<html>` and `&` are automatically escaped by
  `xexpr->string`. Bad for supporting literal HTML in the markdown,
  which is part of
  [Gruber's spec](http://daringfireball.net/projects/markdown/).  Very
  common use case: `<table>`s.  We must detect the HTML textually, and
  recreate as `xexprs`.

The end.

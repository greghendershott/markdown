# TO-DO

- Ordered and unordered lists. See
  [Gruber](http://daringfireball.net/projects/markdown/syntax#list).
  Probably will need to be _block-level_, not intra-block.

- Fix nested literal HTML.

# Notes

This converts to an `xexpr`, not all the way to HTML text.

Pros:

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

Con:

- Things like `<html>` and `&` are automatically escaped by
  `xexpr->string`. Bad for supporting literal HTML in the markdown,
  which is part of
  [Gruber's spec](http://daringfireball.net/projects/markdown/).  Very
  common use case: `<table>`s.  We must detect the HTML textually, and
  recreate as `xexprs`.

/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Dialect
public import Html.Node
public import Html.Escape
public import Html.Attrs
public import Html.Tags
public import Html.Document

/-!
# `Html`: a typed HTML5 library

Renders well-formed, escaped HTML from Lean values, using Lean's type
system to make illegal nesting unrepresentable.

`Node cat` (`Html/Node.lean`) is a tree indexed by the content-model
category it's valid in, with a private constructor: the only way to build
one is through the primitives there, or the tag functions (`Html/Tags.lean`)
built on them, so a well-typed program already has correct nesting and
balanced tags. Attributes reach and are stored by those primitives as an
`Attrs` value (`Html/Escape.lean`) rather than a pre-rendered string, which
leaves `Attrs.render` as the single place names are sanitized and values
escaped, and defers to render time how they are spelled.

## Dialects

`Dialect` (`Html/Dialect.lean`) chooses the serialization, and is an
argument to `Node.render`, `Node.renderPretty`, and `document`, so one tree
can be emitted either way. `.html5`, the default, is the usual HTML syntax.
`.xhtml` is XML: every element either closes or self-closes (`<br />`),
every boolean attribute carries a value (`disabled="disabled"`), and
`document` declares the XHTML namespace on `<html>`.

## Well-formedness

`Node.render_wellFormed` shows that, given no `unsafeRaw` use,
`Node.render` always produces well-formed markup: balanced tags, no
unescaped `<`/`>` outside tag delimiters, and every attribute run of the
shape its dialect admits (`Attrs.render_wellFormed`). `WellFormedHtml` is
indexed by the dialect, and at `.xhtml` has neither an unclosed void tag nor
a bare attribute, so the theorem there says the output is well-formed XML.
It's established compositionally, so `Node.WellFormed` for a concrete tree
follows from chaining the per-constructor lemmas (`element_wellFormed`,
`text_wellFormed`, ...) over however that tree was built, tag functions
included; `test/HtmlTests/Node.lean` works an example. `escape_safe`,
`sanitizeAttrName_safe`, and `Attrs.render_safe` (`Html/Escape.lean`), the
escaping-safety facts that proof rests on, are public too.

There's deliberately no `unsafeRaw_wellFormed`: a `text "hi"` and an
`unsafeRaw "hi"` can be the literal same `Node`, so "was `unsafeRaw` used"
isn't something a theorem can check after the fact, only something a
*construction* can avoid. The theorem covers `Node.render`; it says
nothing about `Node.renderPretty`, whose interposed indentation would need
a side-condition that `unit` contains no `<`/`>`. Nor does it constrain the
`tag` a caller hands `Node.element` directly; the tag functions all pass
literals.

## The one remaining escape hatch

`rawAttrs : List (String × String)`, present on every tag, is safe: names
are sanitized and values escaped, so a name containing e.g. a space or `"`
is neutered rather than breaking out of the tag. The one true escape hatch
is `Node.unsafeRaw : String → Node cat`: verbatim, unescaped markup,
trusted as-is, valid in any category. Misuse can create XSS
vulnerabilities, and it's exactly what `Node.WellFormed` can't see through.
-/

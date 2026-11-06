/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import HtmlRenderer
public import Html.Node
public import Html.Attrs
public import Html.Tags

/-!
# `Html`: a typed HTML5 library

Builds HTML from Lean values, making illegal nesting and illegal child
order type errors.

The tree is lean-html-model's: `Node cat` is a node valid wherever category
`cat` is, whose children were checked against its content model when it was
built, and which widens to any category its own is valid wherever. The tag
functions (`Html/Tags.lean`) build it, with attributes as each element's
payload: an `Attrs` value rather than a pre-rendered string.

`Node`, `Attrs`, rendering, dialects, `document`, the well-formedness
theorem, and `Node.unsafeRaw` come from lean-html-renderer, which this
module re-exports; its documentation describes them.

## Attributes

`rawAttrs : List (String × String)`, present on every tag, is safe: names
are sanitized and values escaped, so a name containing e.g. a space or `"`
is neutered rather than breaking out of the tag. The one true escape hatch
is the renderer's `Node.unsafeRaw`.
-/

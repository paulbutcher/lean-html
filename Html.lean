import Html.Node
import Html.Escape
import Html.Attrs
import Html.Tags
import Html.Document

/-!
# `Html`: a typed HTML5 library

Renders well-formed, escaped HTML from Lean values, using Lean's type
system to make illegal nesting unrepresentable.

## Design overview

`Node (cat : Category)` (`Html/Node.lean`) is a private-constructor
wrapper around a small internal tree (`Repr`), indexed by the HTML
content-model category it's valid in. Only `flow` and `phrasing` are
modeled; a `Coe (Node .phrasing) (Node .flow)` instance lets phrasing
content (`span`, `a`, `strong`, ...) appear directly among a flow
element's children. Every tag function (`Html/Tags.lean`) is a smart
constructor built from `Node`'s public primitives (`element`, `elementOf`,
`voidElement`, `textElement`, `text`), so a well-typed program that builds
a `Node` already has correct tag nesting and balanced tags.

## The two escape hatches, and their safety caveats

Everything above is type-checked and (for escaping) proved safe. Two
escape hatches exist for what the typed vocabulary doesn't cover:

- **`rawAttrs : List (String × String) := []`**, present on every tag.
  Arbitrary `(name, value)` pairs rendered verbatim. *Values* are escaped
  the same way as everything else; *names* are not validated at all -- an
  attribute name containing a space, `=`, or `>` breaks out of the tag
  regardless of value-escaping.
- **`Node.unsafeRaw : String → Node cat`** (`Html/Node.lean`). Verbatim,
  unescaped markup, trusted as-is, valid in any category. Misuse can
  create XSS vulnerabilities.

## How to add a new tag

1. Decide the tag's own `Category` and its children's `Category` (they
   can differ -- e.g. `p` is `flow` but only accepts `phrasing` children,
   which is exactly what makes a `<div>` inside a `<p>` a type error). See
   `Html/Node.lean`'s `element` (same category both sides) vs. `elementOf`
   (different categories) vs. `voidElement` (no children) vs.
   `textElement` (plain-text content, RCDATA-like elements such as
   `<textarea>`).
2. If the element needs attributes beyond the global `HtmlAttrs` (`id`,
   `class`, `style`, `title`, `lang`, `dir`), add a typed record to
   `Html/Attrs.lean` following `AAttrs`/`ImgAttrs`/`InputAttrs`'s pattern:
   `extends HtmlAttrs`, required fields as plain (non-`Option`) fields,
   everything else `Option _ := none`, boolean attributes rendered via
   `renderBoolAttr`, and its `.render` appending `HtmlAttrs.render
   a.toHtmlAttrs` last so the inherited global fields render too.
3. Define the tag function in `Html/Tags.lean`: `(children) (attrs :=
   {}) (rawAttrs := [])`, calling the right `Node` primitive from step 1
   with `combineAttrs <attrs-rendered> rawAttrs` as the attribute string.
4. Add a `#guard` smoke test (minimal render output) to `HtmlTests/Tags.lean`,
   next to the other tags' tests.

## How to add a new attribute

Add a field to `HtmlAttrs` (global) or the relevant per-element record in
`Html/Attrs.lean`, wire it into that structure's `.render`, and add a
`#guard` test to `HtmlTests/Attrs.lean`. Boolean attributes must go through
`renderBoolAttr` (bare name when `true`, absent when `false`, never
`name="false"`); string attributes go through `renderAttr` (escaped,
double-quote-delimited).
-/

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
content-model category it's valid in. Besides `flow`/`phrasing`, `Category`
also carries structure-only categories (`listItem`, `option`/`selectChild`,
`tableColumn`/`tableCell`/`tableRow`/`tableSection`) so that e.g. a `<li>`
or `<tr>` is only accepted where HTML5 actually allows it, not anywhere
flow/phrasing content is. A handful of `Coe` instances (phrasing → flow, a
bare `<option>` → `<select>` child, a bare `<tr>` → `<table>` child) let the
narrower tags appear directly among their wider container's children.
Every tag function (`Html/Tags.lean`) is a smart constructor built from
`Node`'s public primitives (`element`, `elementOf`, `voidElement`,
`textElement`, `text`), so a well-typed program that builds a `Node`
already has correct tag nesting and balanced tags. Attributes flow through
these primitives as an `Attrs` value (`Html/Escape.lean`) -- an ordered
list of name/value or bare-flag fragments -- which `Attrs.render` turns
into a string, sanitizing every name and escaping every value as it goes;
there is no way to hand a constructor a pre-broken attribute string.

## Well-formedness

`Node.render_wellFormed`, proved in `HtmlTests/Node.lean` (not shipped as
part of the `Html` library import -- `Repr`/`renderCompactInto` are exposed
non-`private` specifically so this proof can live in test code rather than
alongside the library itself), shows that, given no `unsafeRaw` use,
`Node.render` always produces well-formed HTML (`WellFormedHtml`: balanced
tags, no unescaped `<`/`>` outside of tag delimiters). It's established
compositionally -- `Node.WellFormed` for a concrete tree follows from
chaining `element_wellFormed`/`elementOf_wellFormed`/`voidElement_wellFormed`/
`textElement_wellFormed`/`text_wellFormed` over however that tree was built
(see the example in `HtmlTests/Node.lean`) -- which is also why there's
deliberately no `unsafeRaw_wellFormed`: a `text "hi"` and an `unsafeRaw "hi"`
can be the literal same `Node` value, so "was `unsafeRaw` used" isn't
something a theorem can check after the fact, only something a
*construction* can avoid. `Node.renderPretty` isn't covered by this theorem
yet -- the argument extends (with an extra side-condition that the
indentation `unit` itself contains no `<`/`>`) but hasn't been carried out.

## The one remaining escape hatch

Everything above is type-checked and (for escaping/well-formedness)
proved safe, including `rawAttrs : List (String × String) := []` (present
on every tag): names are sanitized via `sanitizeAttrName` and values
escaped, so a name containing e.g. a space or `"` is neutered rather than
breaking out of the tag. The one true escape hatch left is
**`Node.unsafeRaw : String → Node cat`** (`Html/Node.lean`): verbatim,
unescaped markup, trusted as-is, valid in any category. Misuse can create
XSS vulnerabilities, and it's exactly what `Node.WellFormed` can't see
through.

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
   everything else `Option _ := none`, and a `.render : ... → Attrs`
   built from `reqAttr`/`optAttr`/`flagAttr`, appending `HtmlAttrs.render
   a.toHtmlAttrs` last so the inherited global fields render too.
3. Define the tag function in `Html/Tags.lean`: `(children) (attrs :=
   {}) (rawAttrs := [])`, calling the right `Node` primitive from step 1
   with `combineAttrs <attrs-rendered> rawAttrs` as the `Attrs` argument.
4. Add a `#guard` smoke test (minimal render output) to `HtmlTests/Tags.lean`,
   next to the other tags' tests.

## How to add a new attribute

Add a field to `HtmlAttrs` (global) or the relevant per-element record in
`Html/Attrs.lean`, wire it into that structure's `.render`, and add a
`#guard` test to `HtmlTests/Attrs.lean`. Boolean attributes go through
`flagAttr` (bare name when `true`, absent when `false`, never
`name="false"`); string attributes go through `optAttr` (when optional) or
`reqAttr` (when required) -- both escaped, double-quote-delimited, via
`Attrs.render`.
-/

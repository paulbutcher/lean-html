# Changelog

## [0.14.2] - 2026-11-06

* Requires html-renderer 0.1.1

## [0.14.1] - 2026-11-06

* The `Html` module documentation matches the README's wording

## [0.14.0] - 2026-11-01

* Rendering, `Node`, `Attrs`, `Dialect`, and `document` come from lean-html-renderer, which `import Html` re-exports
* Breaking: `Html.Escape`, `Html.Render`, `Html.Dialect`, and `Html.Document` are gone; import `Html` or the matching `HtmlRenderer` module instead

## [0.13.0] - 2026-11-01

* Breaking: the tree is lean-html-model's, which checks each element's children against its content model; its own node primitives and content-model classes are gone, along with the per-tag well-formedness lemmas
* Breaking: a list argument is typed by the category its position admits, such as `Node .listItemOrScript` for `ul`, which list items, script-supporting elements, and whitespace widen to
* Breaking: `Node.render_wellFormed` takes `n.hasVerbatim = false` in place of `Node.WellFormed`
* Breaking: `meta_` and `link` return `Node .phrasingMetadata`, and `iframe`, `embed`, and `area` phrasing content
* Text is escaped when it is rendered, not when the tree is built
* `script` or `template` may follow the first `dt` and the first `dd` of a group

## [0.12.0] - 2026-10-30

* Child order and count are typed: `table`, `head`, `details`, `fieldset`, `figure`, `dl`, `picture`, `hgroup`, `ruby`, `colgroup`, `video`, and `audio` take their children as separate arguments, and `document` takes a `head` and a `body`
* `caption`, `dt`, `dd`, `summary`, `legend`, `figcaption`, `title`, `base`, `track`, `rt`, and `rp` each have a category of their own, valid only where their parent allows
* `p`, `h1` to `h6`, `hgroup`, and `img` return the narrower categories `paragraph`, `heading`, `headingGroup`, and `image`, which widen by coercion
* `summary` and `legend` accept heading content
* Text coerces only to phrasing content, `Node.text` returning a `Node .phrasing`, and other categories accept only a `Whitespace` value
* `Node.render` takes `lineBreaks`, which lays out children as cmark does; lean-markdown's GFM tables, which put `"\n"` leaves between table sections, need it instead
* `script`, `template`, `link`, `meta_`, `noscript`, and `source` take their category from their context
* `script` and `template` may stand among list children, `noscript` in `head` holds only `link` and `meta`, and `source` in a `picture` takes `PictureSourceAttrs`
* `src` moves from `VideoAttrs` and `AudioAttrs` to the `MediaSource` argument, `SourceAttrs` requires `src` and loses `srcset` and `media`, `iframe` takes no children, and `picture` is phrasing content
* `Node.widen` replaces `Node.toFlow`, `Node.toSelectChild`, and `Node.toTableSection`, and a category per table part replaces `Category.tableSection`

## [0.11.0] - 2026-10-21

* A repeated attribute name, compared after sanitizing and ignoring ASCII case, now renders once, the first occurrence winning

## [0.10.1] - 2026-10-18

* Chore: Lean version bump

## [0.10.0] - 2026-10-06

* `a`, `canvas`, `video`, `audio`, `object`, `map`, `noscript`, and `slot` take the category of their context, as `ins` and `del` do, so one used standalone needs `(cat := ...)`
* Transparent elements can no longer stand in for a `<li>`, `<tr>`, or `<option>`
* `renderPretty` no longer breaks lines between phrasing siblings, where the break would render as a space

## [0.9.0] - 2026-08-29

* Optional XML output: the new `Dialect` argument to `render`, `renderPretty`, and `document` replaces `selfClosingVoid`, and under `.xhtml` closes every tag, gives every boolean attribute a value, and declares the XHTML namespace
* `Node.render_wellFormed` now proves the attribute run well-formed too, so at `.xhtml` it says the output is XML
* Attribute names starting with a digit, `-`, or `.` are prefixed with `_`, those being legal in a name but not first

## [0.8.1] - 2026-08-29

The build now treats warnings as errors, and the documentation has been tightened.

## [0.8.0] - 2026-08-21

* `Html/Tags.lean` now exposes its definitions, so callers can prove `Node.WellFormed` for trees built from the tag functions
* `WellFormed` now carries across the category coercions

## [0.7.0] - 2026-08-20

Switch to module system

## [0.6.0] - 2026-08-15

Miscellaneous style and layout improvements.

## [0.5.0] - 2026-08-07

`Node.render_wellFormed` and the escaping-safety proofs it depends on are now part of the public `Html` API, so downstream packages can cite them directly.

## [0.4.0] - 2026-08-07

* Formal proof that rendered HTML is always well-formed (`Node.render_wellFormed`)
* Content models for `list`, tables, and `select` restrict nesting to valid children
* Invalid attribute names are now rejected in `rawAttrs`
* Optional XHTML-style self-closing void tags

## [0.3.0] - 2026-08-06

Nicer syntax for building tags and attributes.

## [0.2.0] - 2026-08-06

Add support for previously unsupported attributes.

## [0.1.1] - 2026-07-17

Documentation and packaging fixes.

## [0.1.0] - 2026-07-16

Initial release.

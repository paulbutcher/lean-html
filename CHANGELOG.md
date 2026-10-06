# Changelog

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

# Changelog

## [0.8.0] - 2026-08-21

* `Html/Tags.lean` now exposes its definitions, so callers can prove
  `Node.WellFormed` for trees built from the tag functions
* `Node.toFlow_wellFormed`, `Node.toSelectChild_wellFormed`, and
  `Node.toTableSection_wellFormed` carry `WellFormed` across the category
  coercions

## [0.7.0] - 2026-08-20

Switch to module system

## [0.6.0] - 2026-08-15

Miscellaneous style and layout improvements.

## [0.5.0] - 2026-08-07

* `Node.render_wellFormed` and the escaping-safety proofs it depends on
  (`escape_safe`, `Attrs.render_safe`, ...) are now part of the public
  `Html` API instead of living in test code, so downstream packages can
  cite them directly

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

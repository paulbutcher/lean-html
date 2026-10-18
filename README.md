# HTML

A Lean 4 library for building HTML. It represents markup as typed Lean values so illegal nesting (e.g. a `<div>` inside a `<p>`, or a `<div>` inside a `<ul>`) is a type error rather than a runtime bug, and text content is escaped automatically. `Node.render_wellFormed` proves a structural markup property for nodes built without `Node.unsafeRaw`: balanced tag structure and dialect-appropriate attribute syntax. It does not establish every XML well-formedness condition.

See: [Formally verified CRUD](https://paulbutcher.com/lean2.html).

## Installation

Add it to your `lakefile.toml`:

```toml
[[require]]
name = "html"
git = "https://github.com/paulbutcher/lean-html"
```

## Usage

```lean
import Html

open Html

def page : String :=
  document [
    head [ title "My page" ],
    body [
      h1 [ "Welcome" ],
      p [ "Hello, ", strong [ "world" ], "!" ],
      ul [
        li [ a { href := "/a" } [ "Link A" ] ],
        li [ a { href := "/b" } [ "Link B" ] ],
      ],
    ]
  ] (lang := "en") (pretty := true)
```

`page` is now a complete `<!DOCTYPE html>` document as a `String`.

- Tag functions (`div`, `p`, `a`, `img`, ...) live in the `Html` namespace and mirror standard HTML element names, taking children and a single typed `attrs` record. Every such record includes the global `HtmlAttrs` fields (`id`, `class_`, `style`, `title`, `lang`, `dir`) alongside the element's own, e.g. `a { href := "/x", class_ := "y" }`.
- String literals can be used directly as text content, e.g. `p [ "Hi" ]`.
- `rawAttrs : List (String × String)` is accepted by every tag for attributes not otherwise covered.
- `Node.unsafeRaw : String → Node cat` inserts unescaped markup verbatim when you need it; the content is trusted as-is and not checked, so misuse can introduce XSS vulnerabilities.
- Use `Node.render` for compact output or `Node.renderPretty` for indented output on an individual `Node`; use `document` to produce a full HTML document.
- Pass `(dialect := .xhtml)` to any of the three for XHTML-style serialization instead of HTML5: every element closes or self-closes (`<br />`), every boolean attribute carries a value (`disabled="disabled"`), and `document` declares the XHTML namespace on `<html>`.

## Formal verification

- `Node.render_wellFormed`: given `Node.WellFormed` (which excludes `Node.unsafeRaw`), compact rendering satisfies the inductive `WellFormedHtml` predicate: balanced tag structure, no raw `<` or `>` in text, and dialect-appropriate attribute syntax. In `.xhtml`, void elements self-close and attributes have quoted values. This predicate does not certify complete XML 1.0 well-formedness.
- `escape_safe`: escaped text and attribute values never contain a raw `<`, `>`, or `"`, so neither can break out of the markup around it.
- `sanitizeAttrName_safe` and `sanitizeAttrName_start`: sanitized names satisfy the restricted XML `Name` syntax and cannot inject tag delimiters. These theorems do not restrict which attribute names callers choose or establish a complete HTML security policy.
- `Attrs.render` keeps the first fragment for each attribute name. HTML5 compares ASCII names case-insensitively; XHTML preserves case. `Attrs.render_safe` and `Attrs.render_wellFormed` still describe that run (bare boolean flags under `.html5` only). Unique names are not full XML well-formedness: illegal characters, unbound prefixes, and raw tag names are outside this property.
- `element_wellFormed`, `text_wellFormed`, and their siblings: `Node.WellFormed` is compositional, so you can discharge it for a tree of your own by chaining one lemma per constructor it was built from, tag functions included.

These cover `Node.render`, not `Node.renderPretty`, whose interposed indentation would need a side condition on `unit`. They constrain neither a tag name handed to `Node.element` directly (the tag functions all pass literals) nor `Node.unsafeRaw`, which is trusted by construction.

## License

This library is released under the Apache 2.0 license. See the LICENSE file for the complete license text.

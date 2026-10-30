# HTML

A Lean 4 library for building HTML. It represents markup as typed Lean values so illegal nesting (e.g. a `<div>` inside a `<p>`, or a `<div>` inside a `<ul>`) and illegal child order (e.g. `<thead>` after `<tbody>`) are type errors rather than runtime bugs, and text content is escaped automatically. `Node.render_wellFormed` proves that, as long as you don't reach for `Node.unsafeRaw`, the HTML this library produces is always well-formed.

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
  document
    (head (title "My page") [ meta_ [("charset", "utf-8")] ])
    (body [
      h1 [ "Welcome" ],
      p [ "Hello, ", strong [ "world" ], "!" ],
      ul [
        li [ a { href := "/a" } [ "Link A" ] ],
        li [ a { href := "/b" } [ "Link B" ] ],
      ],
      table [ tr [ td [ "1" ], td [ "2" ] ] ] (thead := thead [ tr [ th [ "A" ], th [ "B" ] ] ]),
    ])
    (lang := "en") (pretty := true)
```

`page` is now a complete `<!DOCTYPE html>` document as a `String`.

- Tag functions (`div`, `p`, `a`, `img`, ...) live in the `Html` namespace and mirror standard HTML element names, taking children and a single typed `attrs` record. Every such record includes the global `HtmlAttrs` fields (`id`, `class_`, `style`, `title`, `lang`, `dir`) alongside the element's own, e.g. `a { href := "/x", class_ := "y" }`.
- String literals can be used directly as phrasing or flow content, e.g. `p [ "Hi" ]`. Elsewhere only a `Whitespace` value is accepted, e.g. `ul [ li [ "a" ], Whitespace.newline, li [ "b" ] ]`.
- `rawAttrs : List (String × String)` is accepted by every tag for attributes not otherwise covered; where a name repeats, ignoring ASCII case, the first occurrence wins, so a typed attribute takes precedence.
- `Node.unsafeRaw : String → Node cat` inserts unescaped markup verbatim when you need it; the content is trusted as-is and not checked, so misuse can introduce XSS vulnerabilities.
- Use `Node.render` for compact output or `Node.renderPretty` for indented output on an individual `Node`; use `document` to produce a full HTML document. `Node.render (lineBreaks := true)` puts each child that isn't phrasing content on a line of its own, as cmark does.
- Pass `(dialect := .xhtml)` to any of the three for XML output instead of HTML5: every element closes or self-closes (`<br />`), every boolean attribute carries a value (`disabled="disabled"`), and `document` declares the XHTML namespace on `<html>`.

## Content model

Each element's children are checked against the HTML Living Standard's content model for it, as far as its direct children go.

- An element valid only inside a particular parent, such as `li`, `dt`, `tr`, `summary`, `legend`, `figcaption`, `caption`, `title`, `source`, or `rt`, has a category of its own, so `div [ dt [ "x" ] ]` is a type error.
- Where the order or count of children is fixed, the tag function takes them as separate arguments: `document head body`, `head title children`, `details summary children`, `picture sources img`, `hgroup before heading after`, and `ruby`, `dl`, `video`, and `audio` through `RubySegment`, `DlContent`, and `MediaSource`.
- Optional parts are named arguments after `attrs`, emitted in the order the standard fixes whatever order they are passed in: `table`'s `caption`, `colgroups`, `thead`, and `tfoot`, `fieldset`'s `legend`, `head`'s `base`, and `figure`'s `caption`, first or last.
- Not modelled: constraints on descendants rather than children, such as no interactive content inside `a` or `button`, no `form` inside `form`, no `header` or `footer` inside `footer`, no `table` inside `caption`, and no media element inside a media element's fallback.
- Nor anything that depends on attributes, such as `link` and `meta` being allowed in the body only with particular attributes, or `rawAttrs` adding a `src` to a `video` with `<source>` children. Some valid markup is rejected: heading content in `summary` or `legend`, `script` or `template` among a table's, list's, or `dl`'s children, and `noscript` in `head`.

## Formal verification

- `Node.render_wellFormed`: given no `Node.unsafeRaw`, rendering always produces balanced tags, with no unescaped `<` or `>` anywhere outside a tag's own delimiters, with or without `lineBreaks`. At `(dialect := .xhtml)` the same theorem says the output is well-formed XML: every tag closed or self-closed, every attribute given a value.
- `escape_safe`: escaped text and attribute values never contain a raw `<`, `>`, or `"`, so neither can break out of the markup around it.
- `sanitizeAttrName_safe` and `sanitizeAttrName_start`: every attribute name is coerced to a valid XML `Name`, which is what makes an untrusted name in `rawAttrs` harmless.
- `Attrs.render_safe` and `Attrs.render_wellFormed`: whatever fragments an `Attrs` value holds, it renders to a run of ` name="value"` pairs with no name repeated, bare boolean flags being admitted under `.html5` only.
- `element_wellFormed`, `text_wellFormed`, and their siblings: `Node.WellFormed` is compositional, so you can discharge it for a tree of your own by chaining one lemma per constructor it was built from, tag functions included; each tag function with structured arguments has its own, such as `table_wellFormed`.

These cover `Node.render`, not `Node.renderPretty`, whose interposed indentation would need a side condition on `unit`. They constrain neither a tag name handed to `Node.element` directly (the tag functions all pass literals) nor `Node.unsafeRaw`, which is trusted by construction.

## License

This library is released under the Apache 2.0 license. See the LICENSE file for the complete license text.

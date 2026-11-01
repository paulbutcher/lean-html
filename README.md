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

Each element's children are checked against the HTML Living Standard's content model for it, as far as its direct children go, by [lean-html-model](https://github.com/paulbutcher/lean-html-model), whose tree this library builds and renders.

- An element valid only inside a particular parent, such as `li`, `dt`, `tr`, `summary`, `legend`, `figcaption`, `caption`, `title`, `source`, or `rt`, has a category of its own, so `div [ dt [ "x" ] ]` is a type error. One that changes with its context takes its category from there: `source` needs `srcset` in a `picture` and `src` in a media element, `noscript` holds only `link` and `meta` in `head`, and `a` and the other transparent elements take flow or phrasing content. A list argument is typed by the category its position admits, which narrower categories widen to: `ul` takes `Node .listItemOrScript`, so an `li`, a `script`, a `template`, and a `Whitespace` value may all stand among its items.
- Where the order or count of children is fixed, the tag function takes them as separate arguments: `document head body`, `head title children`, `details summary children`, `picture sources img`, `hgroup before heading after`, and `ruby`, `dl`, `colgroup`, `video`, and `audio` through `RubySegment`, `DlContent`, `ColgroupContent`, and `MediaSource`.
- Optional parts are named arguments after `attrs`, emitted in the order the standard fixes whatever order they are passed in: `table`'s `caption`, `colgroups`, `thead`, and `tfoot`, `fieldset`'s `legend`, `head`'s `base`, and `figure`'s `caption`, first or last.
- Not modelled: constraints on descendants rather than children, such as no interactive content inside `a` or `button`, no `form` inside `form`, no `header` or `footer` inside `footer`, no `table` inside `caption`, and no media element inside a media element's fallback.
- Nor any other rule that depends on attributes, such as `link` and `meta` being allowed in the body only with particular attributes, or `rawAttrs` adding a `src` to a `video` with `<source>` children. Some valid markup is rejected: `script` or `template` between a table's optional parts, or before the first `dt` or `dd` of a group, where there is no list to hold them.

## Formal verification

- `Node.render_wellFormed`: given no `Node.unsafeRaw`, rendering always produces balanced tags, with no unescaped `<` or `>` anywhere outside a tag's own delimiters, with or without `lineBreaks`. At `(dialect := .xhtml)` the same theorem says the output is well-formed XML: every tag closed or self-closed, every attribute given a value.
- `escape_safe`: escaped text and attribute values never contain a raw `<`, `>`, or `"`, so neither can break out of the markup around it.
- `sanitizeAttrName_safe` and `sanitizeAttrName_start`: every attribute name is coerced to a valid XML `Name`, which is what makes an untrusted name in `rawAttrs` harmless.
- `Attrs.render_safe` and `Attrs.render_wellFormed`: whatever fragments an `Attrs` value holds, it renders to a run of ` name="value"` pairs with no name repeated, bare boolean flags being admitted under `.html5` only.
- `Node.valid_erase`, from lean-html-model: every element's children in a tree satisfy its content model, stated on the erased tree that rendering consumes.

`Node.render_wellFormed` takes as its hypothesis that the node contains no verbatim markup, `n.hasVerbatim = false`, which `decide +kernel` proves for a concrete tree. These cover `Node.render`, not `Node.renderPretty`, whose interposed indentation would need a side condition on `unit`, and they don't constrain `Node.unsafeRaw`, which is trusted by construction.

## License

This library is released under the Apache 2.0 license. See the LICENSE file for the complete license text.

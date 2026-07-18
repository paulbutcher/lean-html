# HTML

A Lean 4 library for building HTML. It represents markup as typed Lean
values so illegal nesting (e.g. a `<div>` inside a `<p>`) is a type error
rather than a runtime bug, and text content is escaped automatically.

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

- Tag functions (`div`, `p`, `a`, `img`, ...) live in the `Html`
  namespace and mirror standard HTML element names, taking children,
  optional typed attributes, and optional `HtmlAttrs` (`id`, `class_`,
  `style`, `title`, `lang`, `dir`).
- String literals can be used directly as text content, e.g. `p [ "Hi" ]`.
- `rawAttrs : List (String × String)` is accepted by every tag for
  attributes not otherwise covered.
- `Node.unsafeRaw : String → Node cat` inserts unescaped markup verbatim
  when you need it — the content is trusted as-is and not checked, so
  misuse can introduce XSS vulnerabilities.
- Use `Node.render` for compact output or `Node.renderPretty` for
  indented output on an individual `Node`; use `document` to produce a
  full HTML document.

## License

This library is released under the Apache 2.0 license. See the LICENSE
file for the complete license text.

import Html.Escape

namespace Html

/-- HTML content-model category. In addition to `flow`/`phrasing`, this
also carries enough structure-only categories to keep list, table, and
`<select>` children from being interchangeable flow content, which they
aren't in real HTML5: a `<li>`, `<tr>`, or `<option>` is only valid inside
its specific parent, not anywhere flow/phrasing content is. What's *not*
modeled is ordering within a parent (e.g. HTML5 wants `<caption>` before
`<colgroup>` before `<thead>` inside `<table>`) -- these categories only
constrain which tags are valid children, not their sequence.
Phrasing content is a subset of flow content, a bare `<option>` is a valid
`<select>` child, and a bare `<tr>` is a valid `<table>` child (the `Coe`
instances below encode exactly that), which is what lets those narrower
tags appear directly among their wider container's children. -/
inductive Category where
  | flow
  | phrasing
  | listItem
  | option
  | selectChild
  | tableColumn
  | tableCell
  | tableRow
  | tableSection

/-- Internal tree representation. Not `private`, so that proofs about
`render`/`renderPretty` (e.g. `HtmlTests/Node.lean`'s well-formedness
theorems) can do induction on it -- but ordinary callers have no reason to
touch it and should go through `Node`'s own constructors instead. -/
inductive Repr where
  | leaf (s : String)
  | void (tag : String) (attrsStr : String)
  | rawText (tag : String) (attrsStr : String) (content : String)
  | elem (tag : String) (attrsStr : String) (children : List Repr) (block : Bool)

/-- A well-typed piece of rendered HTML, indexed by the content-model
category it's valid in. The constructor is private: the only way to build
a `Node` is through `element`/`elementOf`/`voidElement`/`textElement`/
`text`/`unsafeRaw` (and, on top of those, the tag functions in
`Html/Tags.lean`), which is what makes content-model correctness a
corollary of type soundness. The `repr` field is readable (needed by the
same proofs `Repr` is exposed for) but not writable -- `mk` stays private,
so a `Node` still can't be fabricated from an arbitrary `Repr`. -/
structure Node (cat : Category) where
  private mk ::
  repr : Repr

namespace Node

def renderCompactInto : Repr → String → String
  | .leaf s, acc => acc ++ s
  | .void tag attrsStr, acc => acc ++ s!"<{tag}{attrsStr}>"
  | .rawText tag attrsStr content, acc => acc ++ s!"<{tag}{attrsStr}>{content}</{tag}>"
  | .elem tag attrsStr children _, acc =>
    let acc := acc ++ s!"<{tag}{attrsStr}>"
    let acc := children.foldl (fun acc c => renderCompactInto c acc) acc
    acc ++ s!"</{tag}>"

/-- Render a node to an HTML string. -/
def render (n : Node cat) : String := renderCompactInto n.repr ""

private def indent (depth : Nat) (unit : String) : String :=
  String.join (List.replicate depth unit)

private def renderPrettyInto (unit : String) (r : Repr) (depth : Nat) (acc : String) : String :=
  match r with
  | .leaf s => acc ++ s
  | .void tag attrsStr => acc ++ s!"<{tag}{attrsStr}>"
  | .rawText tag attrsStr content => acc ++ s!"<{tag}{attrsStr}>{content}</{tag}>"
  | .elem tag attrsStr children false =>
    -- Inline layout (phrasing children): identical to compact rendering.
    let acc := acc ++ s!"<{tag}{attrsStr}>"
    let acc := children.foldl (fun acc c => renderPrettyInto unit c depth acc) acc
    acc ++ s!"</{tag}>"
  | .elem tag attrsStr [] true =>
    acc ++ s!"<{tag}{attrsStr}></{tag}>"
  | .elem tag attrsStr [.leaf s] true =>
    -- A lone text child isn't worth exploding onto its own line.
    acc ++ s!"<{tag}{attrsStr}>{s}</{tag}>"
  | .elem tag attrsStr [c] true =>
    let acc := acc ++ s!"<{tag}{attrsStr}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit c (depth + 1) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
  | .elem tag attrsStr (c :: cs) true =>
    -- Block layout (flow children), more than one: one child per line.
    let acc := acc ++ s!"<{tag}{attrsStr}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit c (depth + 1) acc
    let acc := cs.foldl (fun acc c =>
      renderPrettyInto unit c (depth + 1) (acc ++ "\n" ++ indent (depth + 1) unit)) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
termination_by sizeOf r

/-- Render a node as indented, human-readable HTML.
`unit` is the string repeated per indentation level (default two spaces). -/
def renderPretty (n : Node cat) (unit : String := "  ") : String :=
  renderPrettyInto unit n.repr 0 ""

/-- A normal element whose children may be a *different*, narrower
category than the element itself -- e.g. `p` is flow content but only
accepts phrasing children (HTML5 disallows a `<div>` directly inside a
`<p>`), which this makes a type error rather than a spec violation caught
only at runtime. `attrs` (built by the tag functions in `Html/Tags.lean`
from `HtmlAttrs.render`/`renderRawAttrs`-style helpers) is rendered to a
string internally via `Attrs.render`, which is what makes the result
well-formed regardless of what names/values a caller supplies -- there's
no way to hand this constructor a pre-broken attribute string the way a
bare `attrsStr : String` parameter would have allowed.
Children are pretty-printed one-per-line (block layout) unless `contentCat`
is `phrasing` -- true inline text-level content -- so the structure-only
categories (`listItem`, `tableRow`, ...) still get block layout like flow
content does, and only genuine prose stays inline. -/
def elementOf (cat contentCat : Category) (tag : String)
    (children : List (Node contentCat)) (attrs : Attrs := []) : Node cat :=
  ⟨.elem tag (Attrs.render attrs) (children.map (·.repr)) (!(contentCat matches .phrasing))⟩

/-- A normal element whose children are the *same* category as the
element itself (e.g. `div`: a flow element containing flow content). -/
def element (cat : Category) (tag : String) (children : List (Node cat))
    (attrs : Attrs := []) : Node cat :=
  elementOf cat cat tag children attrs

/-- A void element: self-closing, takes no children, has no closing tag
(`<br>`, `<img>`, `<input>`, ...). -/
def voidElement (cat : Category) (tag : String) (attrs : Attrs := []) : Node cat :=
  ⟨.void tag (Attrs.render attrs)⟩

/-- An element whose content model is plain text, not nested elements
(`<textarea>`, `<option>` -- these are RCDATA-like in HTML5: entities are
still escaped normally, but `<`/`>` in the content are never parsed as
nested markup, so typing their content as `List (Node cat)` would be
misleading). -/
def textElement (cat : Category) (tag : String) (content : String)
    (attrs : Attrs := []) : Node cat :=
  ⟨.rawText tag (Attrs.render attrs) (escape content)⟩

/-- A leaf of escaped text content, valid in any category (plain text is
both flow and phrasing content). -/
def text (s : String) : Node cat := ⟨.leaf (escape s)⟩

/-- String literals can stand directly for a `text` leaf wherever a `Node`
is expected (e.g. among a tag's children), so callers write `"hi"` instead
of `Node.text "hi"`. -/
instance : Coe String (Node cat) where
  coe := text

/-- Verbatim, unescaped markup, trusted as-is, usable as content of any
category.
Misuse can lead to XSS issues. -/
def unsafeRaw (s : String) : Node cat := ⟨.leaf s⟩

theorem render_text (s : String) : (text s : Node cat).render = escape s := by
  simp [render, text, renderCompactInto]

theorem render_textElement (cat : Category) (tag content : String) (attrs : Attrs) :
    (textElement cat tag content attrs).render
      = s!"<{tag}{Attrs.render attrs}>" ++ escape content ++ s!"</{tag}>" := by
  simp [render, textElement, renderCompactInto, toString, String.append_assoc]

end Node

/-- Phrasing content is always valid wherever flow content is valid. -/
instance : Coe (Node .phrasing) (Node .flow) where
  coe n := ⟨n.repr⟩

/-- A bare `<option>` (without a wrapping `<optgroup>`) is a valid direct
child of `<select>`. -/
instance : Coe (Node .option) (Node .selectChild) where
  coe n := ⟨n.repr⟩

/-- A bare `<tr>` (without a wrapping `<thead>`/`<tbody>`/`<tfoot>`) is a
valid direct child of `<table>`. -/
instance : Coe (Node .tableRow) (Node .tableSection) where
  coe n := ⟨n.repr⟩

end Html

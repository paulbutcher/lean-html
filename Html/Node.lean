import Html.Escape

namespace Html

/-- HTML content-model category. Only `flow` and `phrasing` are currently
modeled.
Phrasing content is a subset of flow content (`Coe` below), which is what
lets a phrasing tag like `span` appear directly among a flow element's
children. -/
inductive Category where
  | flow
  | phrasing

/-- Internal tree representation. -/
private inductive Repr where
  | leaf (s : String)
  | void (tag : String) (attrsStr : String)
  | rawText (tag : String) (attrsStr : String) (content : String)
  | elem (tag : String) (attrsStr : String) (children : List Repr) (block : Bool)

/-- A well-typed piece of rendered HTML, indexed by the content-model
category it's valid in. The constructor is private: the only way to build
a `Node` is through `element`/`elementOf`/`voidElement`/`textElement`/
`text`/`unsafeRaw` (and, on top of those, the tag functions in
`Html/Tags.lean`), which is what makes content-model correctness a
corollary of type soundness. -/
structure Node (cat : Category) where
  private mk ::
  private repr : Repr

namespace Node

private def renderCompactInto : Repr → String → String
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
only at runtime. `attrsStr` is the pre-rendered, already-escaped attribute
string (e.g. from `HtmlAttrs.render` + `renderRawAttrs`, built by the tag
functions in `Html/Tags.lean`), spliced directly after the tag name. -/
def elementOf (cat contentCat : Category) (tag : String)
    (children : List (Node contentCat)) (attrsStr : String := "") : Node cat :=
  ⟨.elem tag attrsStr (children.map (·.repr)) (contentCat matches .flow)⟩

/-- A normal element whose children are the *same* category as the
element itself (e.g. `div`: a flow element containing flow content). -/
def element (cat : Category) (tag : String) (children : List (Node cat))
    (attrsStr : String := "") : Node cat :=
  elementOf cat cat tag children attrsStr

/-- A void element: self-closing, takes no children, has no closing tag
(`<br>`, `<img>`, `<input>`, ...). -/
def voidElement (cat : Category) (tag : String) (attrsStr : String := "") : Node cat :=
  ⟨.void tag attrsStr⟩

/-- An element whose content model is plain text, not nested elements
(`<textarea>`, `<option>` -- these are RCDATA-like in HTML5: entities are
still escaped normally, but `<`/`>` in the content are never parsed as
nested markup, so typing their content as `List (Node cat)` would be
misleading). -/
def textElement (cat : Category) (tag : String) (content : String)
    (attrsStr : String := "") : Node cat :=
  ⟨.rawText tag attrsStr (escape content)⟩

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

end Node

/-- Phrasing content is always valid wherever flow content is valid. -/
instance : Coe (Node .phrasing) (Node .flow) where
  coe n := ⟨n.repr⟩

end Html

/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Escape

public section

namespace Html

/-- HTML content-model category. In addition to `flow`/`phrasing`, this
also carries enough structure-only categories to keep list, table, and
`<select>` children from being interchangeable flow content, which they
aren't in real HTML5: a `<li>`, `<tr>`, or `<option>` is only valid inside
its specific parent, not anywhere flow/phrasing content is. What's *not*
modeled is ordering within a parent (e.g. HTML5 wants `<caption>` before
`<colgroup>` before `<thead>` inside `<table>`); these categories only
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

/-- Internal tree representation. Attributes are held as an `Attrs` list
rather than a rendered string, so that the dialect can still decide how
they're serialized when the tree is rendered. Not `private`, so that
proofs about `render`/`renderPretty` (e.g. this file's well-formedness
theorems) can do induction on it, but ordinary callers have no reason to
touch it and should go through `Node`'s own constructors instead. -/
inductive Repr where
  | leaf (s : String)
  | void (tag : String) (attrs : Attrs)
  | rawText (tag : String) (attrs : Attrs) (content : String)
  | elem (tag : String) (attrs : Attrs) (children : List Repr) (block : Bool)

/-- A well-typed piece of rendered HTML, indexed by the content-model
category it's valid in. The constructor is private: the only way to build
a `Node` is through `element`/`elementOf`/`voidElement`/`textElement`/
`text`/`unsafeRaw` (and, on top of those, the tag functions in
`Html/Tags.lean`), which is what makes content-model correctness a
corollary of type soundness. The `repr` field is readable (needed by the
same proofs `Repr` is exposed for) but not writable; `mk` stays private,
so a `Node` still can't be fabricated from an arbitrary `Repr`. -/
structure Node (cat : Category) where
  private mk ::
  repr : Repr

namespace Node

def renderCompactInto (dialect : Dialect) : Repr → String → String
  | .leaf s, acc => acc ++ s
  | .void tag attrs, acc =>
    acc ++ (match dialect with
      | .html5 => s!"<{tag}{Attrs.render attrs dialect}>"
      | .xhtml => s!"<{tag}{Attrs.render attrs dialect} />")
  | .rawText tag attrs content, acc =>
    acc ++ s!"<{tag}{Attrs.render attrs dialect}>{content}</{tag}>"
  | .elem tag attrs children _, acc =>
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>"
    let acc := children.foldl (fun acc c => renderCompactInto dialect c acc) acc
    acc ++ s!"</{tag}>"

def render (n : Node cat) (dialect : Dialect := .html5) : String :=
  renderCompactInto dialect n.repr ""

private def indent (depth : Nat) (unit : String) : String :=
  String.join (List.replicate depth unit)

private def renderPrettyInto (unit : String) (dialect : Dialect) (r : Repr) (depth : Nat)
    (acc : String) : String :=
  match r with
  | .leaf s => acc ++ s
  | .void tag attrs =>
    acc ++ (match dialect with
      | .html5 => s!"<{tag}{Attrs.render attrs dialect}>"
      | .xhtml => s!"<{tag}{Attrs.render attrs dialect} />")
  | .rawText tag attrs content =>
    acc ++ s!"<{tag}{Attrs.render attrs dialect}>{content}</{tag}>"
  | .elem tag attrs children false =>
    -- Inline layout (phrasing children): identical to compact rendering.
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>"
    let acc := children.foldl (fun acc c => renderPrettyInto unit dialect c depth acc) acc
    acc ++ s!"</{tag}>"
  | .elem tag attrs [] true =>
    acc ++ s!"<{tag}{Attrs.render attrs dialect}></{tag}>"
  | .elem tag attrs [.leaf s] true =>
    -- A lone text child isn't worth exploding onto its own line.
    acc ++ s!"<{tag}{Attrs.render attrs dialect}>{s}</{tag}>"
  | .elem tag attrs [c] true =>
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit dialect c (depth + 1) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
  | .elem tag attrs (c :: cs) true =>
    -- Block layout (flow children), more than one: one child per line.
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit dialect c (depth + 1) acc
    let acc := cs.foldl (fun acc c =>
      renderPrettyInto unit dialect c (depth + 1) (acc ++ "\n" ++ indent (depth + 1) unit)) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
termination_by sizeOf r

/-- Render a node as indented, human-readable HTML.
`unit` is the string repeated per indentation level (default two spaces). -/
def renderPretty (n : Node cat) (unit : String := "  ") (dialect : Dialect := .html5) : String :=
  renderPrettyInto unit dialect n.repr 0 ""

/-- A normal element whose children may be a *different*, narrower
category than the element itself; e.g. `p` is flow content but only
accepts phrasing children (HTML5 disallows a `<div>` directly inside a
`<p>`), which this makes a type error rather than a spec violation caught
only at runtime.
Children are pretty-printed one-per-line (block layout) unless `contentCat`
is `phrasing`, true inline text-level content, so the structure-only
categories (`listItem`, `tableRow`, ...) still get block layout like flow
content does, and only genuine prose stays inline. -/
def elementOf (cat contentCat : Category) (tag : String)
    (children : List (Node contentCat)) (attrs : Attrs := []) : Node cat :=
  ⟨.elem tag attrs (children.map (·.repr)) (!(contentCat matches .phrasing))⟩

/-- A normal element whose children are the *same* category as the
element itself (e.g. `div`: a flow element containing flow content). -/
def element (cat : Category) (tag : String) (children : List (Node cat))
    (attrs : Attrs := []) : Node cat :=
  elementOf cat cat tag children attrs

/-- A void element: takes no children and has no closing tag (`<br>`,
`<img>`, `<input>`, ...), self-closing (`<br />`) under `.xhtml`. -/
def voidElement (cat : Category) (tag : String) (attrs : Attrs := []) : Node cat :=
  ⟨.void tag attrs⟩

/-- An element whose content model is plain text, not nested elements
(`<textarea>`, `<option>`, which are RCDATA-like in HTML5: entities are
still escaped normally, but `<`/`>` in the content are never parsed as
nested markup, so typing their content as `List (Node cat)` would be
misleading). -/
def textElement (cat : Category) (tag : String) (content : String)
    (attrs : Attrs := []) : Node cat :=
  ⟨.rawText tag attrs (escape content)⟩

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

/-- A `text` leaf renders to exactly its escaped content, with nothing
else spliced in. -/
theorem render_text (s : String) (dialect : Dialect := .html5) :
    (text s : Node cat).render dialect = escape s := by
  simp [render, text, renderCompactInto]

/-- A `textElement`'s escaped content is always embedded between its
literal opening and closing tags, so the content can never prematurely
close `</tag>` or open nested markup. -/
theorem render_textElement (cat : Category) (tag content : String) (attrs : Attrs)
    (dialect : Dialect := .html5) :
    (textElement cat tag content attrs).render dialect
      = s!"<{tag}{Attrs.render attrs dialect}>" ++ escape content ++ s!"</{tag}>" := by
  simp [render, textElement, renderCompactInto, toString, String.append_assoc]

/-- Phrasing content is always valid wherever flow content is valid. -/
def toFlow (n : Node .phrasing) : Node .flow := ⟨n.repr⟩

/-- A bare `<option>` (without a wrapping `<optgroup>`) is a valid direct
child of `<select>`. -/
def toSelectChild (n : Node .option) : Node .selectChild := ⟨n.repr⟩

/-- A bare `<tr>` (without a wrapping `<thead>`/`<tbody>`/`<tfoot>`) is a
valid direct child of `<table>`. -/
def toTableSection (n : Node .tableRow) : Node .tableSection := ⟨n.repr⟩

end Node

/-- The well-formed-markup grammar that `render` is proved to produce, given
no `unsafeRaw` use (`Node.render_wellFormed`). A string is well-formed if
every `<` it contains opens a tag immediately matched, once its contents are
exhausted, by a literal `</tag>`, or else opens a void tag: unclosed under
`.html5`, self-closing under `.xhtml`. Every attribute run is a
`WellFormedAttrs` for the same dialect, and nowhere else does a `<` or `>`
appear at all. This is a spec of the *output string*, stated independently of
`Node`/`Repr`; at `.xhtml` it's the XML claim, every tag closed and every
attribute given a value. -/
inductive WellFormedHtml : Dialect → String → Prop where
  | text {d : Dialect} {s : String} (h : ∀ c ∈ s.toList, c ≠ '<' ∧ c ≠ '>') : WellFormedHtml d s
  | void {tag attrsStr : String} (hattrs : WellFormedAttrs .html5 attrsStr) :
      WellFormedHtml .html5 s!"<{tag}{attrsStr}>"
  | selfClosing {tag attrsStr : String} (hattrs : WellFormedAttrs .xhtml attrsStr) :
      WellFormedHtml .xhtml s!"<{tag}{attrsStr} />"
  | elem {d : Dialect} {tag attrsStr inner : String} (hattrs : WellFormedAttrs d attrsStr)
      (hinner : WellFormedHtml d inner) :
      WellFormedHtml d (s!"<{tag}{attrsStr}>" ++ inner ++ s!"</{tag}>")
  | append {d : Dialect} {a b : String} (ha : WellFormedHtml d a) (hb : WellFormedHtml d b) :
      WellFormedHtml d (a ++ b)

namespace Node

/-- Folding a "rebase-able" `g` (one where feeding it a starting
accumulator is the same as prepending that accumulator to what `g`
produces from `""`) over a list is itself rebase-able: running the fold
from `acc` is the same as prepending `acc` to running it from `""`. Used
to pull `renderCompactInto`'s threaded accumulator out from under a
`List.foldl` over a node's children. -/
private theorem foldl_rebase {α : Type} (g : α → String → String) :
    (l : List α) → (∀ x ∈ l, ∀ (s : String), g x s = s ++ g x "") →
      ∀ acc : String, l.foldl (fun acc x => g x acc) acc = acc ++ l.foldl (fun acc x => g x acc) ""
  | [], _, _ => by simp
  | x :: xs, hg, acc => by
    have hgx : ∀ s, g x s = s ++ g x "" := hg x (List.mem_cons_self ..)
    have hgxs : ∀ y ∈ xs, ∀ s, g y s = s ++ g y "" := fun y hy => hg y (List.mem_cons_of_mem _ hy)
    simp only [List.foldl_cons]
    rw [hgx acc, foldl_rebase g xs hgxs (acc ++ g x ""),
        foldl_rebase g xs hgxs (g x ""), String.append_assoc]

/-- `renderCompactInto`'s threaded accumulator is just a prefix: rendering
into an existing accumulator is the same as prepending that accumulator to
what rendering from scratch produces. -/
private theorem renderCompactInto_append (dialect : Dialect) (r : Repr) (acc : String) :
    renderCompactInto dialect r acc = acc ++ renderCompactInto dialect r "" :=
  match r with
  | .leaf _ => by simp [renderCompactInto]
  | .void _ _ => by cases dialect <;> simp [renderCompactInto]
  | .rawText _ _ _ => by simp [renderCompactInto, String.append_assoc]
  | .elem tag attrs children _ => by
    simp only [renderCompactInto, String.empty_append]
    have hg : ∀ c ∈ children, ∀ s,
        renderCompactInto dialect c s = s ++ renderCompactInto dialect c "" :=
      fun c _ s => renderCompactInto_append dialect c s
    rw [foldl_rebase (renderCompactInto dialect) children hg
          (acc ++ s!"<{tag}{Attrs.render attrs dialect}>"),
        foldl_rebase (renderCompactInto dialect) children hg
          (s!"<{tag}{Attrs.render attrs dialect}>")]
    simp [String.append_assoc]

/-- A `Repr` built without `unsafeRaw`: every leaf/text string it carries
avoids raw `<`/`>`. Attributes need no condition here, since an `Attrs`
value can't carry one through `Attrs.render` (`Attrs.render_wellFormed`).
This can't be checked after the fact for an arbitrary `Node` (a `text "hi"`
and an `unsafeRaw "hi"` are literally the same value once built), so
`WellFormed` below isn't a runtime check; it's established compositionally,
per constructor, by the lemmas that follow (`text_wellFormed`,
`elementOf_wellFormed`, ...), each of which mirrors a step of building a
`Node` without ever calling `unsafeRaw`. There's deliberately no
`unsafeRaw_wellFormed`. -/
private inductive WellFormedRepr : Repr → Prop where
  | leaf {s : String} (h : ∀ c ∈ s.toList, c ≠ '<' ∧ c ≠ '>') : WellFormedRepr (.leaf s)
  | void {tag : String} {attrs : Attrs} : WellFormedRepr (.void tag attrs)
  | rawText {tag : String} {attrs : Attrs} {content : String}
      (hcontent : ∀ c ∈ content.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedRepr (.rawText tag attrs content)
  | elem {tag : String} {attrs : Attrs} {children : List Repr} {block : Bool}
      (hchildren : ∀ c ∈ children, WellFormedRepr c) :
      WellFormedRepr (.elem tag attrs children block)

/-- A `Node` built without `unsafeRaw`; see `WellFormedRepr`. -/
def WellFormed (n : Node cat) : Prop := WellFormedRepr n.repr

private theorem foldl_wellFormed (dialect : Dialect) (children : List Repr)
    (hind : ∀ c ∈ children, ∀ acc, WellFormedHtml dialect acc →
      WellFormedHtml dialect (renderCompactInto dialect c acc)) :
    ∀ acc, WellFormedHtml dialect acc →
      WellFormedHtml dialect
        (children.foldl (fun acc c => renderCompactInto dialect c acc) acc) := by
  induction children with
  | nil => intro acc hacc; simpa using hacc
  | cons c cs ih =>
    intro acc hacc
    simp only [List.foldl_cons]
    exact ih (fun c' hc' => hind c' (List.mem_cons_of_mem _ hc'))
      (renderCompactInto dialect c acc) (hind c (List.mem_cons_self ..) acc hacc)

private theorem renderCompactInto_wellFormed (dialect : Dialect) (r : Repr)
    (hr : WellFormedRepr r) :
    ∀ acc, WellFormedHtml dialect acc →
      WellFormedHtml dialect (renderCompactInto dialect r acc) := by
  induction hr with
  | leaf h => intro acc hacc; simp only [renderCompactInto]; exact hacc.append (.text h)
  | @void tag attrs =>
    intro acc hacc
    cases dialect <;> simp only [renderCompactInto]
    · exact hacc.append (.void (tag := tag) (Attrs.render_wellFormed attrs .html5))
    · exact hacc.append (.selfClosing (tag := tag) (Attrs.render_wellFormed attrs .xhtml))
  | rawText hcontent =>
    intro acc hacc
    simp only [renderCompactInto]
    simpa [toString, String.append_assoc] using
      hacc.append (.elem (Attrs.render_wellFormed _ dialect) (.text hcontent))
  | elem hchildren ih =>
    rename_i tag attrs children block
    intro acc hacc
    simp only [renderCompactInto]
    have hg : ∀ c ∈ children, ∀ s,
        renderCompactInto dialect c s = s ++ renderCompactInto dialect c "" :=
      fun c _ s => renderCompactInto_append dialect c s
    rw [foldl_rebase (renderCompactInto dialect) children hg
      (acc ++ s!"<{tag}{Attrs.render attrs dialect}>")]
    have hinner : WellFormedHtml dialect
        (children.foldl (fun acc c => renderCompactInto dialect c acc) "") :=
      foldl_wellFormed dialect children ih "" (.text (by simp))
    simpa [String.append_assoc] using
      hacc.append (.elem (Attrs.render_wellFormed attrs dialect) hinner)

/-- **The well-formedness theorem:** given no `unsafeRaw` use, `render`
always produces well-formed markup (`WellFormedHtml`): balanced tags, no
unescaped `<`/`>` anywhere outside of tag delimiters, and every attribute
run of the shape its dialect admits. At `.xhtml` that output is XML. -/
theorem render_wellFormed (n : Node cat) (h : WellFormed n) (dialect : Dialect := .html5) :
    WellFormedHtml dialect (n.render dialect) :=
  renderCompactInto_wellFormed dialect n.repr h "" (.text (by simp))

/-- Drops the `≠ '"'` conjunct `escape_safe` gives, down to what
`WellFormed`/`WellFormedHtml` need. -/
private theorem escape_safe' (s : String) : ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' :=
  fun c hc => ⟨(escape_safe s c hc).1, (escape_safe s c hc).2.1⟩

theorem text_wellFormed (s : String) : WellFormed (text s : Node cat) :=
  .leaf (escape_safe' s)

theorem textElement_wellFormed (cat : Category) (tag content : String) (attrs : Attrs) :
    WellFormed (textElement cat tag content attrs) :=
  .rawText (escape_safe' content)

theorem voidElement_wellFormed (cat : Category) (tag : String) (attrs : Attrs) :
    WellFormed (voidElement cat tag attrs) :=
  .void

/-- **The compositional heart of the well-formedness theorem:** an element
built from already-`WellFormed` children, via the typed constructors (never
`unsafeRaw`), is itself `WellFormed`. Chaining this (and `element_wellFormed`,
`voidElement_wellFormed`, `textElement_wellFormed`, `text_wellFormed`) over
however a concrete `Node` was assembled gives `WellFormed` for that `Node`,
which `render_wellFormed` then turns into `WellFormedHtml` for its
rendering. -/
theorem elementOf_wellFormed (cat contentCat : Category) (tag : String)
    (children : List (Node contentCat)) (attrs : Attrs) (h : ∀ c ∈ children, WellFormed c) :
    WellFormed (elementOf cat contentCat tag children attrs) :=
  .elem fun r hr => by
    obtain ⟨c, hc, hceq⟩ := List.mem_map.mp hr
    exact hceq ▸ h c hc

theorem element_wellFormed (cat : Category) (tag : String) (children : List (Node cat))
    (attrs : Attrs) (h : ∀ c ∈ children, WellFormed c) : WellFormed (element cat tag children attrs) :=
  elementOf_wellFormed cat cat tag children attrs h

/-- The category coercions carry `WellFormed` across, so a phrasing child
placed among flow content (and likewise for `<option>`/`<tr>`) needs no
separate proof. -/
theorem toFlow_wellFormed {n : Node .phrasing} (h : WellFormed n) : WellFormed n.toFlow := h

theorem toSelectChild_wellFormed {n : Node .option} (h : WellFormed n) :
    WellFormed n.toSelectChild := h

theorem toTableSection_wellFormed {n : Node .tableRow} (h : WellFormed n) :
    WellFormed n.toTableSection := h

end Node

-- Instance bodies are exposed to importers, so they cannot mention `Node.mk`;
-- each coercion goes through one of the named widening functions above instead.
instance : Coe (Node .phrasing) (Node .flow) := ⟨Node.toFlow⟩
instance : Coe (Node .option) (Node .selectChild) := ⟨Node.toSelectChild⟩
instance : Coe (Node .tableRow) (Node .tableSection) := ⟨Node.toTableSection⟩

end Html

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

/-- Internal tree representation. Not `private`, so that proofs about
`render`/`renderPretty` (e.g. this file's well-formedness theorems) can do
induction on it, but ordinary callers have no reason to touch it and
should go through `Node`'s own constructors instead. -/
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
same proofs `Repr` is exposed for) but not writable; `mk` stays private,
so a `Node` still can't be fabricated from an arbitrary `Repr`. -/
structure Node (cat : Category) where
  private mk ::
  repr : Repr

namespace Node

/-- `selfClosingVoid` selects XHTML-style void tags (`<br />`) over the
default HTML5 style (`<br>`), a whole-document serialization convention,
not a per-tag choice, so it's a render-time parameter rather than something
recorded on `Repr.void` itself. -/
def renderCompactInto (selfClosingVoid : Bool) : Repr → String → String
  | .leaf s, acc => acc ++ s
  | .void tag attrsStr, acc =>
    acc ++ (if selfClosingVoid then s!"<{tag}{attrsStr} />" else s!"<{tag}{attrsStr}>")
  | .rawText tag attrsStr content, acc => acc ++ s!"<{tag}{attrsStr}>{content}</{tag}>"
  | .elem tag attrsStr children _, acc =>
    let acc := acc ++ s!"<{tag}{attrsStr}>"
    let acc := children.foldl (fun acc c => renderCompactInto selfClosingVoid c acc) acc
    acc ++ s!"</{tag}>"

def render (n : Node cat) (selfClosingVoid : Bool := false) : String :=
  renderCompactInto selfClosingVoid n.repr ""

private def indent (depth : Nat) (unit : String) : String :=
  String.join (List.replicate depth unit)

private def renderPrettyInto (unit : String) (selfClosingVoid : Bool) (r : Repr) (depth : Nat)
    (acc : String) : String :=
  match r with
  | .leaf s => acc ++ s
  | .void tag attrsStr =>
    acc ++ (if selfClosingVoid then s!"<{tag}{attrsStr} />" else s!"<{tag}{attrsStr}>")
  | .rawText tag attrsStr content => acc ++ s!"<{tag}{attrsStr}>{content}</{tag}>"
  | .elem tag attrsStr children false =>
    -- Inline layout (phrasing children): identical to compact rendering.
    let acc := acc ++ s!"<{tag}{attrsStr}>"
    let acc := children.foldl (fun acc c => renderPrettyInto unit selfClosingVoid c depth acc) acc
    acc ++ s!"</{tag}>"
  | .elem tag attrsStr [] true =>
    acc ++ s!"<{tag}{attrsStr}></{tag}>"
  | .elem tag attrsStr [.leaf s] true =>
    -- A lone text child isn't worth exploding onto its own line.
    acc ++ s!"<{tag}{attrsStr}>{s}</{tag}>"
  | .elem tag attrsStr [c] true =>
    let acc := acc ++ s!"<{tag}{attrsStr}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit selfClosingVoid c (depth + 1) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
  | .elem tag attrsStr (c :: cs) true =>
    -- Block layout (flow children), more than one: one child per line.
    let acc := acc ++ s!"<{tag}{attrsStr}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit selfClosingVoid c (depth + 1) acc
    let acc := cs.foldl (fun acc c =>
      renderPrettyInto unit selfClosingVoid c (depth + 1) (acc ++ "\n" ++ indent (depth + 1) unit)) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
termination_by sizeOf r

/-- Render a node as indented, human-readable HTML.
`unit` is the string repeated per indentation level (default two spaces). -/
def renderPretty (n : Node cat) (unit : String := "  ") (selfClosingVoid : Bool := false) : String :=
  renderPrettyInto unit selfClosingVoid n.repr 0 ""

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
(`<textarea>`, `<option>`, which are RCDATA-like in HTML5: entities are
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

/-- A `text` leaf renders to exactly its escaped content, with nothing
else spliced in. -/
theorem render_text (s : String) : (text s : Node cat).render = escape s := by
  simp [render, text, renderCompactInto]

/-- A `textElement`'s escaped content is always embedded between its
literal opening and closing tags, so the content can never prematurely
close `</tag>` or open nested markup. -/
theorem render_textElement (cat : Category) (tag content : String) (attrs : Attrs) :
    (textElement cat tag content attrs).render
      = s!"<{tag}{Attrs.render attrs}>" ++ escape content ++ s!"</{tag}>" := by
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

/-- The well-formed-HTML grammar that `render`/`renderPretty` are proved to
produce, given no `unsafeRaw` use (`Node.render_wellFormed`). A string is
well-formed if every `<` it contains either opens a void tag `<tag attrs>`
with nothing else following, or opens a tag immediately matched, once its
contents are exhausted, by a literal `</tag>`; and nowhere else does a
`<` or `>` appear at all. This is a spec of the *output string*, stated
independently of `Node`/`Repr`. -/
inductive WellFormedHtml (selfClosingVoid : Bool) : String → Prop where
  | text {s : String} (h : ∀ c ∈ s.toList, c ≠ '<' ∧ c ≠ '>') : WellFormedHtml selfClosingVoid s
  | void {tag attrsStr : String} (h : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedHtml selfClosingVoid
        (if selfClosingVoid then s!"<{tag}{attrsStr} />" else s!"<{tag}{attrsStr}>")
  | elem {tag attrsStr inner : String}
      (hattrs : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>') (hinner : WellFormedHtml selfClosingVoid inner) :
      WellFormedHtml selfClosingVoid (s!"<{tag}{attrsStr}>" ++ inner ++ s!"</{tag}>")
  | append {a b : String} (ha : WellFormedHtml selfClosingVoid a) (hb : WellFormedHtml selfClosingVoid b) :
      WellFormedHtml selfClosingVoid (a ++ b)

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
private theorem renderCompactInto_append (selfClosingVoid : Bool) (r : Repr) (acc : String) :
    renderCompactInto selfClosingVoid r acc = acc ++ renderCompactInto selfClosingVoid r "" :=
  match r with
  | .leaf _ => by simp [renderCompactInto]
  | .void _ _ => by simp [renderCompactInto]
  | .rawText _ _ _ => by simp [renderCompactInto, String.append_assoc]
  | .elem tag attrsStr children _ => by
    simp only [renderCompactInto, String.empty_append]
    have hg : ∀ c ∈ children, ∀ s,
        renderCompactInto selfClosingVoid c s = s ++ renderCompactInto selfClosingVoid c "" :=
      fun c _ s => renderCompactInto_append selfClosingVoid c s
    rw [foldl_rebase (renderCompactInto selfClosingVoid) children hg (acc ++ s!"<{tag}{attrsStr}>"),
        foldl_rebase (renderCompactInto selfClosingVoid) children hg (s!"<{tag}{attrsStr}>")]
    simp [String.append_assoc]

/-- A `Repr` built without `unsafeRaw`: every leaf/text/attribute string it
carries avoids raw `<`/`>`. This can't be checked after the fact for an
arbitrary `Node` (a `text "hi"` and an `unsafeRaw "hi"` are literally the
same value once built), so `WellFormed` below isn't a runtime check; it's
established compositionally, per constructor, by the lemmas that follow
(`text_wellFormed`, `elementOf_wellFormed`, ...), each of which mirrors a
step of building a `Node` without ever calling `unsafeRaw`. There's
deliberately no `unsafeRaw_wellFormed`. -/
private inductive WellFormedRepr : Repr → Prop where
  | leaf {s : String} (h : ∀ c ∈ s.toList, c ≠ '<' ∧ c ≠ '>') : WellFormedRepr (.leaf s)
  | void {tag attrsStr : String} (h : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedRepr (.void tag attrsStr)
  | rawText {tag attrsStr content : String}
      (hattrs : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>')
      (hcontent : ∀ c ∈ content.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedRepr (.rawText tag attrsStr content)
  | elem {tag attrsStr : String} {children : List Repr} {block : Bool}
      (hattrs : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>')
      (hchildren : ∀ c ∈ children, WellFormedRepr c) :
      WellFormedRepr (.elem tag attrsStr children block)

/-- A `Node` built without `unsafeRaw`; see `WellFormedRepr`. -/
def WellFormed (n : Node cat) : Prop := WellFormedRepr n.repr

private theorem foldl_wellFormed (selfClosingVoid : Bool) (children : List Repr)
    (hind : ∀ c ∈ children, ∀ acc, WellFormedHtml selfClosingVoid acc →
      WellFormedHtml selfClosingVoid (renderCompactInto selfClosingVoid c acc)) :
    ∀ acc, WellFormedHtml selfClosingVoid acc →
      WellFormedHtml selfClosingVoid (children.foldl (fun acc c => renderCompactInto selfClosingVoid c acc) acc) := by
  induction children with
  | nil => intro acc hacc; simpa using hacc
  | cons c cs ih =>
    intro acc hacc
    simp only [List.foldl_cons]
    exact ih (fun c' hc' => hind c' (List.mem_cons_of_mem _ hc'))
      (renderCompactInto selfClosingVoid c acc) (hind c (List.mem_cons_self ..) acc hacc)

private theorem renderCompactInto_wellFormed (selfClosingVoid : Bool) (r : Repr) (hr : WellFormedRepr r) :
    ∀ acc, WellFormedHtml selfClosingVoid acc →
      WellFormedHtml selfClosingVoid (renderCompactInto selfClosingVoid r acc) := by
  induction hr with
  | leaf h => intro acc hacc; simp only [renderCompactInto]; exact hacc.append (.text h)
  | void h => intro acc hacc; simp only [renderCompactInto]; exact hacc.append (.void h)
  | rawText hattrs hcontent =>
    intro acc hacc
    simp only [renderCompactInto]
    simpa [toString, String.append_assoc] using hacc.append (.elem hattrs (.text hcontent))
  | elem hattrs hchildren ih =>
    rename_i tag attrsStr children block
    intro acc hacc
    simp only [renderCompactInto]
    have hg : ∀ c ∈ children, ∀ s,
        renderCompactInto selfClosingVoid c s = s ++ renderCompactInto selfClosingVoid c "" :=
      fun c _ s => renderCompactInto_append selfClosingVoid c s
    rw [foldl_rebase (renderCompactInto selfClosingVoid) children hg (acc ++ s!"<{tag}{attrsStr}>")]
    have hinner : WellFormedHtml selfClosingVoid
        (children.foldl (fun acc c => renderCompactInto selfClosingVoid c acc) "") :=
      foldl_wellFormed selfClosingVoid children ih "" (.text (by simp))
    simpa [String.append_assoc] using hacc.append (.elem hattrs hinner)

/-- **The well-formedness theorem:** given no `unsafeRaw` use, `render`
always produces well-formed HTML (`WellFormedHtml`): balanced tags, with
no unescaped `<`/`>` anywhere outside of tag delimiters. -/
theorem render_wellFormed (n : Node cat) (h : WellFormed n) (selfClosingVoid : Bool := false) :
    WellFormedHtml selfClosingVoid (n.render selfClosingVoid) :=
  renderCompactInto_wellFormed selfClosingVoid n.repr h "" (.text (by simp))

/-- Drops the `≠ '"'` conjunct `escape_safe` gives, down to what
`WellFormed`/`WellFormedHtml` need. -/
private theorem escape_safe' (s : String) : ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' :=
  fun c hc => ⟨(escape_safe s c hc).1, (escape_safe s c hc).2.1⟩

theorem text_wellFormed (s : String) : WellFormed (text s : Node cat) :=
  .leaf (escape_safe' s)

theorem textElement_wellFormed (cat : Category) (tag content : String) (attrs : Attrs) :
    WellFormed (textElement cat tag content attrs) :=
  .rawText (Attrs.render_safe attrs) (escape_safe' content)

theorem voidElement_wellFormed (cat : Category) (tag : String) (attrs : Attrs) :
    WellFormed (voidElement cat tag attrs) :=
  .void (Attrs.render_safe attrs)

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
  .elem (Attrs.render_safe attrs) fun r hr => by
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

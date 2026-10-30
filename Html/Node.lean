/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Escape

public section

namespace Html

/-- HTML content-model category. Besides `flow`/`phrasing`, there is a category for each
element whose parent fixes where it may appear: a `<li>`, `<dt>`, `<tr>`, `<summary>`,
`<title>`, and so on is valid only in the argument of its parent's tag function that the content
model reserves for it. Where that argument is an `Option`, a single node, or a sum type, order
and count are enforced too.
`heading` (`h1`-`h6`), `headingGroup` (`hgroup`), `paragraph` (`p`), and `image` (`img`) are
flow or phrasing content narrowed so that `hgroup` and `picture` can demand exactly them;
`Subcategory` widens them, and the other subset relations, back out. `phrasingOrHeading` is what
`summary` and `legend` hold, and `noscriptMetadata` what a `noscript` in `<head>` holds. -/
inductive Category where
  | flow
  | phrasing
  | phrasingOrHeading
  | heading
  | headingGroup
  | paragraph
  | image
  | metadata
  | noscriptMetadata
  | title
  | base
  | head
  | body
  | listItem
  | term
  | description
  | descriptionGroup
  | figureCaption
  | summary
  | legend
  | source
  | pictureSource
  | track
  | rubyText
  | rubyParenthesis
  | option
  | selectChild
  | tableCaption
  | tableColumnGroup
  | tableColumn
  | tableHead
  | tableBody
  | tableFoot
  | tableRow
  | tableCell

/-- Whether content of this category lays out inline, so that neither the pretty-printer nor
`lineBreaks` may put whitespace beside it. `rt` and `rp` count, since whitespace between them and
the base text would render inside the ruby. -/
def Category.isPhrasing : Category → Bool
  | .phrasing | .image | .rubyText | .rubyParenthesis => true
  | _ => false

/-- `a` is valid wherever `b` is, so a `Node a` coerces to a `Node b`. -/
class Subcategory (a b : Category) : Prop

instance : Subcategory .phrasing .flow := ⟨⟩
instance : Subcategory .phrasing .phrasingOrHeading := ⟨⟩
instance : Subcategory .heading .flow := ⟨⟩
instance : Subcategory .heading .phrasingOrHeading := ⟨⟩
instance : Subcategory .headingGroup .flow := ⟨⟩
instance : Subcategory .headingGroup .phrasingOrHeading := ⟨⟩
instance : Subcategory .paragraph .flow := ⟨⟩
instance : Subcategory .image .phrasing := ⟨⟩
instance : Subcategory .image .flow := ⟨⟩
instance : Subcategory .option .selectChild := ⟨⟩

/-- The categories an element with HTML5's transparent content model (`a`, `ins`, `canvas`,
...) can take on from its context, so it can sit in a `<p>` or wrap a `<p>` in a `<div>`. Such
an element is itself flow or phrasing content, so it can't stand in for a `<li>`, `<tr>`, or
`<option>`. -/
class Transparent (cat : Category) : Prop

instance : Transparent .flow := ⟨⟩
instance : Transparent .phrasing := ⟨⟩

/-- The categories `link` and `meta` can take on: `<head>` content, including a `<noscript>`
there, and flow or phrasing content in the body. -/
class Metadata (cat : Category) : Prop

instance : Metadata .metadata := ⟨⟩
instance : Metadata .noscriptMetadata := ⟨⟩
instance : Metadata .flow := ⟨⟩
instance : Metadata .phrasing := ⟨⟩

/-- The categories that admit inter-element whitespace: those of a `List` argument, but never one
that also fills a required argument, where whitespace would stand in for the element
required. -/
class InterElement (cat : Category) : Prop

instance : InterElement .flow := ⟨⟩
instance : InterElement .phrasing := ⟨⟩
instance : InterElement .phrasingOrHeading := ⟨⟩
instance : InterElement .paragraph := ⟨⟩
instance : InterElement .metadata := ⟨⟩
instance : InterElement .noscriptMetadata := ⟨⟩
instance : InterElement .listItem := ⟨⟩
instance : InterElement .descriptionGroup := ⟨⟩
instance : InterElement .source := ⟨⟩
instance : InterElement .pictureSource := ⟨⟩
instance : InterElement .track := ⟨⟩
instance : InterElement .option := ⟨⟩
instance : InterElement .selectChild := ⟨⟩
instance : InterElement .tableColumnGroup := ⟨⟩
instance : InterElement .tableColumn := ⟨⟩
instance : InterElement .tableBody := ⟨⟩
instance : InterElement .tableRow := ⟨⟩
instance : InterElement .tableCell := ⟨⟩

/-- The categories `script` can take on: metadata, flow, and phrasing content, and those of a
`List` argument whose parent lets script-supporting elements intermix with its children. As with
`InterElement`, a category that also fills a required argument is left out, so a script can't
stand in for a required `<dt>` or `<dd>`. -/
class ScriptContext (cat : Category) : Prop

instance : ScriptContext .metadata := ⟨⟩
instance : ScriptContext .flow := ⟨⟩
instance : ScriptContext .phrasing := ⟨⟩
instance : ScriptContext .phrasingOrHeading := ⟨⟩
instance : ScriptContext .paragraph := ⟨⟩
instance : ScriptContext .listItem := ⟨⟩
instance : ScriptContext .descriptionGroup := ⟨⟩
instance : ScriptContext .pictureSource := ⟨⟩
instance : ScriptContext .option := ⟨⟩
instance : ScriptContext .selectChild := ⟨⟩
instance : ScriptContext .tableColumnGroup := ⟨⟩
instance : ScriptContext .tableBody := ⟨⟩
instance : ScriptContext .tableRow := ⟨⟩
instance : ScriptContext .tableCell := ⟨⟩

/-- The categories `template` can take on: wherever `script` can, and among a `<colgroup>`'s
`<col>` children, which admit `template` but not `script`. -/
class TemplateContext (cat : Category) : Prop

instance [ScriptContext cat] : TemplateContext cat := ⟨⟩
instance : TemplateContext .tableColumn := ⟨⟩

/-- HTML's ASCII whitespace. -/
@[expose] def isAsciiWhitespace (c : Char) : Bool :=
  c == ' ' || c == '\t' || c == '\n' || c == '\x0C' || c == '\r'

/-- A string of ASCII whitespace, the only text valid outside phrasing content. For a literal,
`{ val := "\n" }` discharges the proof by `decide`. -/
structure Whitespace where
  val : String
  isWhitespace : val.toList.all isAsciiWhitespace := by decide

namespace Whitespace

def newline : Whitespace := { val := "\n" }

def ofString? (s : String) : Option Whitespace :=
  if h : s.toList.all isAsciiWhitespace then some ⟨s, h⟩ else none

end Whitespace

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
  /-- `block` is set when some child isn't phrasing content. Its children are then grouped so
  that every run of phrasing siblings, even a run of one, is a single `run`. -/
  | elem (tag : String) (attrs : Attrs) (children : List Repr) (block : Bool)
  /-- Adjacent phrasing siblings among block content, rendered as just their concatenation;
  the pretty-printer keeps them on one line. -/
  | run (children : List Repr)

def Repr.isRun : Repr → Bool
  | .run _ => true
  | _ => false

/-- A well-typed piece of rendered HTML, indexed by the content-model
category it's valid in. The constructor is private: the only way to build
a `Node` is through `element`/`elementOf`/`elementSeq`/`transparentElement`/
`voidElement`/`textElement`/`text`/`whitespace`/`unsafeRaw` (and, on top of those, the tag
functions in `Html/Tags.lean`), which is what makes content-model correctness a
corollary of type soundness. The `repr` field is readable (needed by the
same proofs `Repr` is exposed for) but not writable; `mk` stays private,
so a `Node` still can't be fabricated from an arbitrary `Repr`. -/
structure Node (cat : Category) where
  private mk ::
  repr : Repr
  /-- Always true of a `Node .phrasing`, but a `Node .flow` may have been widened from one,
  and its parent's layout needs to know which. -/
  isPhrasing : Bool

/-- A node of any category, for an element whose content model fixes the order of children
drawn from several categories. -/
abbrev Child := (cat : Category) × Node cat

namespace Node

def child (n : Node cat) : Child := ⟨cat, n⟩

/-- `lineBreaks` puts each child that isn't phrasing content on a line of its own within a
parent that has any such child, beginning a line only where one hasn't already begun, which is
the convention cmark follows. -/
def renderCompactInto (dialect : Dialect) (lineBreaks : Bool) : Repr → String → String
  | .leaf s, acc => acc ++ s
  | .void tag attrs, acc =>
    acc ++ (match dialect with
      | .html5 => s!"<{tag}{Attrs.render attrs dialect}>"
      | .xhtml => s!"<{tag}{Attrs.render attrs dialect} />")
  | .rawText tag attrs content, acc =>
    acc ++ s!"<{tag}{Attrs.render attrs dialect}>{content}</{tag}>"
  | .elem tag attrs children block, acc =>
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>"
    let acc := children.foldl (fun acc c =>
      if lineBreaks && block && !c.isRun then
        renderCompactInto dialect lineBreaks c
          (if acc.endsWith "\n" then acc else acc ++ "\n") ++ "\n"
      else renderCompactInto dialect lineBreaks c acc) acc
    acc ++ s!"</{tag}>"
  | .run children, acc =>
    children.foldl (fun acc c => renderCompactInto dialect lineBreaks c acc) acc

/-- Render a node as compact HTML. With `lineBreaks`, each child that isn't phrasing content
starts a line and ends one, as in cmark's output; the node itself gets no line break around
it. -/
def render (n : Node cat) (dialect : Dialect := .html5) (lineBreaks : Bool := false) : String :=
  renderCompactInto dialect lineBreaks n.repr ""

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
  | .elem tag attrs [c] true =>
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit dialect c (depth + 1) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
  | .elem tag attrs (c :: cs) true =>
    -- Block layout, more than one child: one per line.
    let acc := acc ++ s!"<{tag}{Attrs.render attrs dialect}>\n" ++ indent (depth + 1) unit
    let acc := renderPrettyInto unit dialect c (depth + 1) acc
    let acc := cs.foldl (fun acc c =>
      renderPrettyInto unit dialect c (depth + 1) (acc ++ "\n" ++ indent (depth + 1) unit)) acc
    acc ++ "\n" ++ indent depth unit ++ s!"</{tag}>"
  | .run children => children.foldl (fun acc c => renderPrettyInto unit dialect c depth acc) acc
termination_by sizeOf r

/-- Render a node as indented, human-readable HTML.
`unit` is the string repeated per indentation level (default two spaces). Line breaks go only
beside a child that isn't phrasing content, since between phrasing siblings they would
render. -/
def renderPretty (n : Node cat) (unit : String := "  ") (dialect : Dialect := .html5) : String :=
  renderPrettyInto unit dialect n.repr 0 ""

private def flushRun : List Repr → List Repr
  | [] => []
  | rs => [.run rs.reverse]

/-- `pending` holds the current run of phrasing siblings, most recent first. Each child is
paired with whether it is phrasing content. -/
private def gatherRuns (pending : List Repr) : List (Repr × Bool) → List Repr
  | [] => flushRun pending
  | (r, true) :: cs => gatherRuns (r :: pending) cs
  | (r, false) :: cs => flushRun pending ++ r :: gatherRuns [] cs

/-- Inline layout if every child is phrasing content, else one line per child, except that
adjacent phrasing children share a line: whitespace between phrasing siblings renders, so the
pretty-printer must never put any there. -/
private def layout (tag : String) (attrs : Attrs) (children : List (Repr × Bool)) : Repr :=
  if children.all (·.2) then .elem tag attrs (children.map (·.1)) false
  else .elem tag attrs (gatherRuns [] children) true

private def part (n : Node cat) : Repr × Bool := (n.repr, n.isPhrasing)

/-- A normal element whose children may be a *different*, narrower
category than the element itself; e.g. `p` is flow content but only
accepts phrasing children (HTML5 disallows a `<div>` directly inside a
`<p>`), which this makes a type error rather than a spec violation caught
only at runtime. -/
def elementOf (cat contentCat : Category) (tag : String)
    (children : List (Node contentCat)) (attrs : Attrs := []) : Node cat :=
  ⟨layout tag attrs (children.map part), cat.isPhrasing⟩

/-- A normal element whose children are the *same* category as the
element itself (e.g. `div`: a flow element containing flow content). -/
def element (cat : Category) (tag : String) (children : List (Node cat))
    (attrs : Attrs := []) : Node cat :=
  elementOf cat cat tag children attrs

/-- An element whose children come from several categories, in the order its tag function
fixes (e.g. `table`: a caption, then column groups, then sections). -/
def elementSeq (cat : Category) (tag : String) (children : List Child)
    (attrs : Attrs := []) : Node cat :=
  ⟨layout tag attrs (children.map (part ·.2)), cat.isPhrasing⟩

/-- An element with HTML5's transparent content model (`ins`, `del`, ...), which takes on the
category of its context. One holding only phrasing content is phrasing content itself, even
among flow content. `before` precedes the transparent children without affecting that, as a
media element's `<source>` and `<track>` children do. -/
def transparentElement (cat : Category) [Transparent cat] (tag : String)
    (children : List (Node cat)) (attrs : Attrs := []) (before : List Child := []) : Node cat :=
  ⟨layout tag attrs (before.map (part ·.2) ++ children.map part), children.all (·.isPhrasing)⟩

/-- A void element: takes no children and has no closing tag (`<br>`,
`<img>`, `<input>`, ...), self-closing (`<br />`) under `.xhtml`. -/
def voidElement (cat : Category) (tag : String) (attrs : Attrs := []) : Node cat :=
  ⟨.void tag attrs, cat.isPhrasing⟩

/-- An element whose content model is plain text, not nested elements
(`<textarea>`, `<option>`, which are RCDATA-like in HTML5: entities are
still escaped normally, but `<`/`>` in the content are never parsed as
nested markup, so typing their content as `List (Node cat)` would be
misleading). -/
def textElement (cat : Category) (tag : String) (content : String)
    (attrs : Attrs := []) : Node cat :=
  ⟨.rawText tag attrs (escape content), cat.isPhrasing⟩

/-- A leaf of escaped text. Text is phrasing content, and reaches flow content by widening. -/
def text (s : String) : Node .phrasing := ⟨.leaf (escape s), true⟩

/-- String literals can stand directly for a `text` leaf wherever phrasing or flow content is
expected (e.g. among a tag's children), so callers write `"hi"` instead of `Node.text "hi"`. -/
instance : Coe String (Node .phrasing) where
  coe := text

/-- Inter-element whitespace, the only text valid among children that aren't phrasing
content. -/
def whitespace [InterElement cat] (w : Whitespace) : Node cat := ⟨.leaf w.val, true⟩

instance [InterElement cat] : Coe Whitespace (Node cat) where
  coe := whitespace

/-- Verbatim, unescaped markup, trusted as-is, usable as content of any
category.
Misuse can lead to XSS issues. -/
def unsafeRaw (s : String) : Node cat :=
  -- Counted as phrasing whatever it holds, since inline layout never alters what renders.
  ⟨.leaf s, true⟩

/-- A `text` leaf renders to exactly its escaped content, with nothing
else spliced in. -/
theorem render_text (s : String) (dialect : Dialect := .html5) (lineBreaks : Bool := false) :
    (text s).render dialect lineBreaks = escape s := by
  simp [render, text, renderCompactInto]

/-- A `textElement`'s escaped content is always embedded between its
literal opening and closing tags, so the content can never prematurely
close `</tag>` or open nested markup. -/
theorem render_textElement (cat : Category) (tag content : String) (attrs : Attrs)
    (dialect : Dialect := .html5) (lineBreaks : Bool := false) :
    (textElement cat tag content attrs).render dialect lineBreaks
      = s!"<{tag}{Attrs.render attrs dialect}>" ++ escape content ++ s!"</{tag}>" := by
  simp [render, textElement, renderCompactInto, toString, String.append_assoc]

/-- A node of a narrower category is valid wherever the wider one is. -/
def widen [Subcategory a b] (n : Node a) : Node b := ⟨n.repr, n.isPhrasing⟩

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
  | run {children : List Repr} (hchildren : ∀ c ∈ children, WellFormedRepr c) :
      WellFormedRepr (.run children)

/-- A `Node` built without `unsafeRaw`; see `WellFormedRepr`. -/
def WellFormed (n : Node cat) : Prop := WellFormedRepr n.repr

/-- What rendering appends to the accumulator is itself well-formed. The accumulator matters
only through `lineBreaks`, which inspects its last character, so this is stated for every
accumulator rather than as rendering from `""` and prepending. -/
private def Appends (dialect : Dialect) (f : String → String) : Prop :=
  ∀ acc, ∃ y, f acc = acc ++ y ∧ WellFormedHtml dialect y

private theorem empty_wellFormed (dialect : Dialect) : WellFormedHtml dialect "" :=
  .text (by simp)

private theorem foldl_appends {α : Type} (dialect : Dialect) (g : α → String → String) :
    (l : List α) → (∀ x ∈ l, Appends dialect (g x)) →
      Appends dialect (fun acc => l.foldl (fun acc x => g x acc) acc)
  | [], _, acc => ⟨"", by simp, empty_wellFormed dialect⟩
  | x :: xs, hg, acc => by
    obtain ⟨y₁, h₁, hy₁⟩ := hg x (List.mem_cons_self ..) acc
    obtain ⟨y₂, h₂, hy₂⟩ := foldl_appends dialect g xs
      (fun y hy => hg y (List.mem_cons_of_mem _ hy)) (acc ++ y₁)
    refine ⟨y₁ ++ y₂, ?_, hy₁.append hy₂⟩
    simp only [List.foldl_cons, h₁, h₂, String.append_assoc]

private theorem renderCompactInto_appends (dialect : Dialect) (lineBreaks : Bool) (r : Repr)
    (hr : WellFormedRepr r) : Appends dialect (renderCompactInto dialect lineBreaks r) := by
  induction hr with
  | leaf h => intro acc; exact ⟨_, by simp [renderCompactInto], .text h⟩
  | @void tag attrs =>
    intro acc
    cases dialect
    · exact ⟨_, by simp [renderCompactInto], .void (tag := tag) (Attrs.render_wellFormed attrs .html5)⟩
    · exact ⟨_, by simp [renderCompactInto],
        .selfClosing (tag := tag) (Attrs.render_wellFormed attrs .xhtml)⟩
  | @rawText tag attrs content hcontent =>
    intro acc
    refine ⟨s!"<{tag}{Attrs.render attrs dialect}>" ++ content ++ s!"</{tag}>",
      by simp [renderCompactInto, toString, String.append_assoc], ?_⟩
    exact .elem (Attrs.render_wellFormed attrs dialect) (.text hcontent)
  | @elem tag attrs children block _ ih =>
    intro acc
    have hstep : ∀ c ∈ children, Appends dialect (fun acc =>
        if lineBreaks && block && !c.isRun then
          renderCompactInto dialect lineBreaks c
            (if acc.endsWith "\n" then acc else acc ++ "\n") ++ "\n"
        else renderCompactInto dialect lineBreaks c acc) := by
      intro c hc acc
      have newline : WellFormedHtml dialect "\n" := .text (by simp)
      dsimp only
      split
      · split
        · obtain ⟨y, h, hy⟩ := ih c hc acc
          exact ⟨y ++ "\n", by rw [h, String.append_assoc], hy.append newline⟩
        · obtain ⟨y, h, hy⟩ := ih c hc (acc ++ "\n")
          exact ⟨"\n" ++ y ++ "\n", by simp [h, String.append_assoc],
            (newline.append hy).append newline⟩
      · exact ih c hc acc
    obtain ⟨y, h, hy⟩ := foldl_appends dialect _ children hstep
      (acc ++ s!"<{tag}{Attrs.render attrs dialect}>")
    refine ⟨s!"<{tag}{Attrs.render attrs dialect}>" ++ y ++ s!"</{tag}>", ?_,
      .elem (Attrs.render_wellFormed attrs dialect) hy⟩
    simp only at h
    simp only [renderCompactInto]
    rw [h]
    simp only [String.append_assoc]
  | run _ ih =>
    intro acc
    simpa [renderCompactInto] using foldl_appends dialect _ _ ih acc

/-- **The well-formedness theorem:** given no `unsafeRaw` use, `render`
always produces well-formed markup (`WellFormedHtml`): balanced tags, no
unescaped `<`/`>` anywhere outside of tag delimiters, and every attribute
run of the shape its dialect admits. At `.xhtml` that output is XML. -/
theorem render_wellFormed (n : Node cat) (h : WellFormed n) (dialect : Dialect := .html5)
    (lineBreaks : Bool := false) : WellFormedHtml dialect (n.render dialect lineBreaks) := by
  obtain ⟨y, hy, hwf⟩ := renderCompactInto_appends dialect lineBreaks n.repr h ""
  simpa [render, hy] using hwf

/-- Drops the `≠ '"'` conjunct `escape_safe` gives, down to what
`WellFormed`/`WellFormedHtml` need. -/
private theorem escape_safe' (s : String) : ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' :=
  fun c hc => ⟨(escape_safe s c hc).1, (escape_safe s c hc).2.1⟩

theorem text_wellFormed (s : String) : WellFormed (text s) :=
  .leaf (escape_safe' s)

theorem whitespace_wellFormed [InterElement cat] (w : Whitespace) :
    WellFormed (whitespace w : Node cat) :=
  .leaf fun c hc => by
    have h := List.all_eq_true.mp w.isWhitespace c hc
    constructor <;> rintro rfl <;> simp [isAsciiWhitespace] at h

theorem textElement_wellFormed (cat : Category) (tag content : String) (attrs : Attrs) :
    WellFormed (textElement cat tag content attrs) :=
  .rawText (escape_safe' content)

theorem voidElement_wellFormed (cat : Category) (tag : String) (attrs : Attrs) :
    WellFormed (voidElement cat tag attrs) :=
  .void

private theorem flushRun_wellFormed (rs : List Repr) (h : ∀ r ∈ rs, WellFormedRepr r) :
    ∀ r ∈ flushRun rs, WellFormedRepr r := by
  match rs, h with
  | [], _ => simp [flushRun]
  | r :: rs, h =>
    intro r' hr'
    simp only [flushRun, List.mem_singleton] at hr'
    subst hr'
    exact .run fun c hc => h c (List.mem_reverse.mp hc)

private theorem gatherRuns_wellFormed (pending : List Repr) (children : List (Repr × Bool))
    (hp : ∀ r ∈ pending, WellFormedRepr r) (hc : ∀ c ∈ children, WellFormedRepr c.1) :
    ∀ r ∈ gatherRuns pending children, WellFormedRepr r := by
  induction children generalizing pending with
  | nil => exact flushRun_wellFormed pending hp
  | cons c cs ih =>
    have hcs : ∀ c ∈ cs, WellFormedRepr c.1 := fun c' h => hc c' (List.mem_cons_of_mem _ h)
    have hc₀ : WellFormedRepr c.1 := hc c (List.mem_cons_self ..)
    obtain ⟨r₀, b⟩ := c
    cases b
    · intro r hr
      simp only [gatherRuns, List.mem_append, List.mem_cons] at hr
      rcases hr with h | h | h
      · exact flushRun_wellFormed pending hp r h
      · exact h ▸ hc₀
      · exact ih [] (by simp) hcs r h
    · refine ih (r₀ :: pending) ?_ hcs
      intro r hr
      cases List.mem_cons.mp hr with
      | inl h => exact h ▸ hc₀
      | inr h => exact hp r h

private theorem layout_wellFormed (tag : String) (attrs : Attrs) (children : List (Repr × Bool))
    (h : ∀ c ∈ children, WellFormedRepr c.1) : WellFormedRepr (layout tag attrs children) := by
  unfold layout
  split
  · exact .elem fun r hr => by
      obtain ⟨c, hc, hceq⟩ := List.mem_map.mp hr
      exact hceq ▸ h c hc
  · exact .elem (gatherRuns_wellFormed [] children (by simp) h)

private theorem parts_wellFormed (children : List (Node cat)) (h : ∀ c ∈ children, WellFormed c) :
    ∀ p ∈ children.map part, WellFormedRepr p.1 := by
  intro p hp
  obtain ⟨c, hc, rfl⟩ := List.mem_map.mp hp
  exact h c hc

private theorem childParts_wellFormed (children : List Child)
    (h : ∀ c ∈ children, WellFormed c.2) : ∀ p ∈ children.map (part ·.2), WellFormedRepr p.1 := by
  intro p hp
  obtain ⟨c, hc, rfl⟩ := List.mem_map.mp hp
  exact h c hc

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
  layout_wellFormed tag attrs _ (parts_wellFormed children h)

theorem element_wellFormed (cat : Category) (tag : String) (children : List (Node cat))
    (attrs : Attrs) (h : ∀ c ∈ children, WellFormed c) : WellFormed (element cat tag children attrs) :=
  elementOf_wellFormed cat cat tag children attrs h

def WellFormedChildren (children : List Child) : Prop := ∀ c ∈ children, WellFormed c.2

theorem WellFormedChildren.nil : WellFormedChildren [] := by simp [WellFormedChildren]

theorem WellFormedChildren.cons {n : Node cat} {cs : List Child} (h : WellFormed n)
    (hs : WellFormedChildren cs) : WellFormedChildren (n.child :: cs) := by
  intro c hc
  rcases List.mem_cons.mp hc with rfl | hc
  · exact h
  · exact hs c hc

theorem WellFormedChildren.append {as bs : List Child} (ha : WellFormedChildren as)
    (hb : WellFormedChildren bs) : WellFormedChildren (as ++ bs) := by
  intro c hc
  rcases List.mem_append.mp hc with hc | hc
  · exact ha c hc
  · exact hb c hc

theorem WellFormedChildren.ofNodes {ns : List (Node cat)} (h : ∀ n ∈ ns, WellFormed n) :
    WellFormedChildren (ns.map child) := by
  intro c hc
  obtain ⟨n, hn, rfl⟩ := List.mem_map.mp hc
  exact h n hn

theorem WellFormedChildren.ofOption {o : Option (Node cat)} (h : ∀ n ∈ o, WellFormed n) :
    WellFormedChildren (o.toList.map child) :=
  ofNodes fun n hn => h n (by simpa using hn)

theorem WellFormedChildren.flatMap {α : Type} {xs : List α} {f : α → List Child}
    (h : ∀ x ∈ xs, WellFormedChildren (f x)) : WellFormedChildren (xs.flatMap f) := by
  intro c hc
  obtain ⟨x, hx, hc⟩ := List.mem_flatMap.mp hc
  exact h x hx c hc

theorem elementSeq_wellFormed (cat : Category) (tag : String) (children : List Child)
    (attrs : Attrs) (h : WellFormedChildren children) :
    WellFormed (elementSeq cat tag children attrs : Node cat) :=
  layout_wellFormed tag attrs _ (childParts_wellFormed children h)

theorem transparentElement_wellFormed (cat : Category) [Transparent cat] (tag : String)
    (children : List (Node cat)) (attrs : Attrs) (before : List Child)
    (hbefore : WellFormedChildren before) (h : ∀ c ∈ children, WellFormed c) :
    WellFormed (transparentElement cat tag children attrs before) :=
  layout_wellFormed tag attrs _ fun p hp => by
    rcases List.mem_append.mp hp with hp | hp
    · exact childParts_wellFormed before hbefore p hp
    · exact parts_wellFormed children h p hp

/-- Widening carries `WellFormed` across, so a phrasing child placed among flow content (and
likewise for the other subcategories) needs no separate proof. -/
theorem widen_wellFormed [Subcategory a b] {n : Node a} (h : WellFormed n) :
    WellFormed (n.widen : Node b) := h

end Node

-- Instance bodies are exposed to importers, so they cannot mention `Node.mk`. One instance per
-- `Subcategory` pair, since `Coe` searches from its target and so can't solve for the source.
instance : Coe (Node .phrasing) (Node .flow) := ⟨Node.widen⟩
instance : Coe (Node .phrasing) (Node .phrasingOrHeading) := ⟨Node.widen⟩
instance : Coe (Node .heading) (Node .flow) := ⟨Node.widen⟩
instance : Coe (Node .heading) (Node .phrasingOrHeading) := ⟨Node.widen⟩
instance : Coe (Node .headingGroup) (Node .flow) := ⟨Node.widen⟩
instance : Coe (Node .headingGroup) (Node .phrasingOrHeading) := ⟨Node.widen⟩
instance : Coe (Node .paragraph) (Node .flow) := ⟨Node.widen⟩
instance : Coe (Node .image) (Node .phrasing) := ⟨Node.widen⟩
instance : Coe (Node .image) (Node .flow) := ⟨Node.widen⟩
instance : Coe (Node .option) (Node .selectChild) := ⟨Node.widen⟩

end Html

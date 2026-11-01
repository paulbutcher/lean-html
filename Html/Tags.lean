/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Node
public import HtmlRenderer.Escape
public import Html.Attrs

/-!
Named tag functions, building lean-html-model elements with `Html/Attrs.lean`'s typed attribute
vocabulary as their payload.

Scope notes:

- `html` is **not** defined here; it's inseparable from the
  `<!DOCTYPE html>` prefix that makes it a document at all, so it stays
  `Html.document`'s sole responsibility rather than a general-purpose tag.
- Tags with well-known attributes beyond the global set get a dedicated
  `<Tag>Attrs` record in `Html/Attrs.lean` (`AAttrs`, `ImgAttrs`,
  `InputAttrs`, and ~35 others), which `extends HtmlAttrs` so a single
  `attrs` argument carries both. Tags with nothing beyond the global set,
  such as `div`, `p`, `span`, `legend`, `summary`, and `datalist`, take
  plain `HtmlAttrs` directly, plus `rawAttrs`, which also covers any
  attribute not modeled as a typed field on a tag that does have a record.
- Where the content model fixes the order or count of an element's children, its tag function
  takes them as separate arguments: an `Option` for at most one, a `List` for any number, a
  single node for exactly one, and a sum type (`TableBody`, `DlContent`, `FigureCaption`,
  `MediaSource`, `RubyText`) for alternatives. An optional element always follows `attrs` and
  `rawAttrs`, and is passed by name.
- A `List` argument is typed by the category its position admits, which a node of any narrower
  category widens to: `ul` takes `Node .listItemOrScript`, which an `li`, a `script`, a
  `template`, and inter-element whitespace all coerce to. Between a table's optional parts, and
  before the first `<dt>` or `<dd>` of a group, the spec admits script-supporting elements too but
  there is no list to hold them, so they are rejected.
-/

@[expose] public section

namespace Html

open HtmlModel

def combineAttrs (specific : Attrs) (rawAttrs : List (String × String)) : Attrs :=
  specific ++ rawAttrs.map fun (n, v) => .value n v

/-- An element of category `cat` whose children are one list of category `content`. -/
abbrev listElement (e : HtmlModel.Html.Elem) (cat content : Category) (children : List (Node content))
    (attrs : Attrs) (hc : Web.Cat.html cat ∈ Web.vocab.cats (.html e) := by decide +kernel)
    (ok : Web.vocab.Admits (.html e) (.html cat) [.many (.html content)] := by admits) : Node cat :=
  Web.elem (.html e) (.html cat) attrs segs![.many (Nodes.ofList children)] hc ok

/-- An element of category `cat` with no children. -/
abbrev voidElement (e : HtmlModel.Html.Elem) (cat : Category) (attrs : Attrs)
    (hc : Web.Cat.html cat ∈ Web.vocab.cats (.html e) := by decide +kernel)
    (ok : Web.vocab.Admits (.html e) (.html cat) [] := by admits) : Node cat :=
  Web.elem (.html e) (.html cat) attrs segs![] hc ok

/-- An element of category `cat` whose content is plain text, not nested elements. -/
abbrev textElement (e : HtmlModel.Html.Elem) (cat : Category) (content : String) (attrs : Attrs)
    (hc : Web.Cat.html cat ∈ Web.vocab.cats (.html e) := by decide +kernel)
    (ok : Web.vocab.Admits (.html e) (.html cat) [.one .text] := by admits) : Node cat :=
  Web.elem (.html e) (.html cat) attrs segs![.one (.text content)] hc ok

/-- The categories an element with HTML5's transparent content model (`a`, `ins`, `canvas`,
...) can take on from its context, so it can sit in a `<p>` or wrap a `<p>` in a `<div>`. -/
class Transparent (cat : Category) : Prop where
  cases : cat = .flow ∨ cat = .phrasing

instance : Transparent .flow := ⟨.inl rfl⟩
instance : Transparent .phrasing := ⟨.inr rfl⟩

/-- A transparent element: one list of children of the category it takes. -/
abbrev transparentElement (e : HtmlModel.Html.Elem) (cat : Category) [Transparent cat]
    (children : List (Node cat)) (attrs : Attrs)
    (hc : Web.Cat.html cat ∈ Web.vocab.cats (.html e))
    (ok : Web.vocab.Admits (.html e) (.html cat) [.many (.html cat)]) : Node cat :=
  listElement e cat cat children attrs hc ok

-- Structure: flow content, flow children.
def div (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .div .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def section_ (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .section .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def article (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .article .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def header (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .header .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def footer (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .footer .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def nav (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .nav .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def aside (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .aside .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Any number of `<p>`, then one heading, then any number of `<p>`. -/
def hgroup (before : List (Node .paragraphOrScript)) (heading : Node .heading)
    (after : List (Node .paragraphOrScript) := []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .headingGroup :=
  Web.elem (.html .hgroup) (.html .headingGroup) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    segs![.many (Nodes.ofList before), .one heading, .many (Nodes.ofList after)]

def address (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .address .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def main (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .main .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def search (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .search .flow .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Exactly one `<title>` and at most one `<base>`. `base` renders first, so that it precedes
every URL it resolves; `title` renders last, so that a long one can't push a
`<meta charset>` among `children` out of the first 1024 bytes. -/
def head (title : Node .title) (children : List (Node .metadata) := []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) (base : Option (Node .base) := none) : Node .head :=
  Web.elem (.html .head) (.html .head) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    segs![Seg.ofOption _ base, .many (Nodes.ofList children), .one title]

def body (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .body :=
  listElement .body .body .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def title (content : String) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .title :=
  textElement .title .title content (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Takes `rawAttrs` as its primary content rather than a typed
attrs record, since a meta tag's shape varies by purpose:
`[("charset", "utf-8")]`, `[("name", "viewport"), ("content", "...")]`,
`[("http-equiv", "..."), ("content", "...")]`, and so on, with no one
shape common enough to single out as required fields. -/
def meta_ (rawAttrs : List (String × String)) (attrs : HtmlAttrs := {}) : Node .phrasingMetadata :=
  voidElement .meta .phrasingMetadata (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def link (attrs : LinkAttrs) (rawAttrs : List (String × String) := []) : Node .phrasingMetadata :=
  voidElement .link .phrasingMetadata (combineAttrs (LinkAttrs.render attrs) rawAttrs)

def base (attrs : BaseAttrs := {}) (rawAttrs : List (String × String) := []) : Node .base :=
  voidElement .base .base (combineAttrs (BaseAttrs.render attrs) rawAttrs)

def script (attrs : ScriptAttrs) (rawAttrs : List (String × String) := []) :
    Node .scriptSupporting :=
  Web.elem (.html .script) (.html .scriptSupporting)
    (combineAttrs (ScriptAttrs.render attrs) rawAttrs) segs![]

/-- What a `<noscript>` in context `cat` holds: transparent content in the body, but only `link`,
`meta`, and `style` in `<head>`. A function of the context, not a class's output, so that the
children are elaborated against it rather than fixing it. -/
abbrev noscriptContent : Category → Category
  | .metadata => .noscriptMetadata
  | cat => cat

/-- The contexts a `<noscript>` may stand in. -/
class NoscriptContext (cat : Category) : Prop where
  takes : Web.Cat.html cat ∈ Web.vocab.cats (.html .noscript)
  admits : Web.vocab.Admits (.html .noscript) (.html cat) [.many (.html (noscriptContent cat))]

instance : NoscriptContext .metadata := ⟨by decide, by decide +kernel⟩
instance : NoscriptContext .flow := ⟨by decide, by decide +kernel⟩
instance : NoscriptContext .phrasing := ⟨by decide, by decide +kernel⟩

def noscript {cat : Category} [h : NoscriptContext cat] (children : List (Node (noscriptContent cat)))
    (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  listElement .noscript cat (noscriptContent cat) children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs) h.takes h.admits

def template (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .template :=
  listElement .template .template .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def canvas {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : CanvasAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .canvas cat children (combineAttrs (CanvasAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def slot {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : SlotAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .slot cat children (combineAttrs (SlotAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def p (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .paragraph :=
  listElement .p .paragraph .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h1 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  listElement .h1 .heading .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h2 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  listElement .h2 .heading .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h3 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  listElement .h3 .heading .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h4 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  listElement .h4 .heading .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h5 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  listElement .h5 .heading .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h6 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  listElement .h6 .heading .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ul (children : List (Node .listItemOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .ul .flow .listItemOrScript children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ol (children : List (Node .listItemOrScript)) (attrs : OlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .ol .flow .listItemOrScript children (combineAttrs (OlAttrs.render attrs) rawAttrs)

def li (children : List (Node .flow)) (attrs : LiAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .listItem :=
  listElement .li .listItem .flow children (combineAttrs (LiAttrs.render attrs) rawAttrs)

def menu (children : List (Node .listItemOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .menu .flow .listItemOrScript children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- One or more `<dt>` followed by one or more `<dd>`. -/
structure DlGroup where
  term : Node .term
  moreTerms : List (Node .termOrScript) := []
  desc : Node .description
  moreDescs : List (Node .descriptionOrScript) := []

abbrev DlGroup.kinds : List (SegK Web.Cat) :=
  [.one (.html .term), .many (.html .termOrScript), .one (.html .description),
    .many (.html .descriptionOrScript)]

def DlGroup.segs (g : DlGroup) : Segs Web.vocab.sig Attrs DlGroup.kinds :=
  segs![.one g.term, .many (Nodes.ofList g.moreTerms), .one g.desc,
    .many (Nodes.ofList g.moreDescs)]

/-- A `<dl>` holds either bare groups or groups each wrapped in a `<div>`, never a mixture. -/
inductive DlContent where
  | groups (groups : List DlGroup)
  | divs (divs : List (Node .descriptionGroupOrScript))

instance : Coe (List DlGroup) DlContent := ⟨.groups⟩
instance : Coe (List (Node .descriptionGroupOrScript)) DlContent := ⟨.divs⟩
instance : Coe (List (Node .descriptionGroup)) DlContent :=
  ⟨fun ds => .divs (ds.map fun (d : Node .descriptionGroup) => (d : Node .descriptionGroupOrScript))⟩

def dl (content : DlContent := .groups []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  let children : Seg Web.vocab.sig Attrs
      (.alt [.groups DlGroup.kinds] [.many (.html .descriptionGroupOrScript)]) :=
    match content with
    | .groups gs => .inl segs![.groups (Groups.ofList (gs.map DlGroup.segs))]
    | .divs ds => .inr segs![.many (Nodes.ofList ds)]
  Web.elem (.html .dl) (.html .flow) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    segs![children]

/-- A `<div>` wrapping one group of a `<dl>`. -/
def dlDiv (group : DlGroup) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .descriptionGroup :=
  Web.elem (.html .div) (.html .descriptionGroup) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    group.segs

def dt (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .term :=
  listElement .dt .term .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dd (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .description :=
  listElement .dd .description .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def blockquote (children : List (Node .flow)) (attrs : BlockquoteAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .blockquote .flow .flow children (combineAttrs (BlockquoteAttrs.render attrs) rawAttrs)

/-- Where a `<figure>`'s caption goes, if it has one: its first child or its last. -/
inductive FigureCaption where
  | none
  | first (caption : Node .figureCaption)
  | last (caption : Node .figureCaption)

def figure (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) (caption : FigureCaption := .none) : Node .flow :=
  let children := Nodes.ofList children
  let content : Seg Web.vocab.sig Attrs
      (.alt [.one (.html .figureCaption), .many (.html .flow)]
        [.many (.html .flow), .opt (.html .figureCaption)]) :=
    match caption with
    | .none => .inr segs![.many children, .none]
    | .first c => .inl segs![.one c, .many children]
    | .last c => .inr segs![.many children, .some c]
  Web.elem (.html .figure) (.html .flow) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    segs![content]

def figcaption (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .figureCaption :=
  listElement .figcaption .figureCaption .flow children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

-- `pre`: flow content, phrasing-only children (preformatted text).
def pre (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .pre .flow .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def code (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .code .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def a {cat : Category} [Transparent cat] (attrs : AAttrs) (children : List (Node cat))
    (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .a cat children (combineAttrs (AAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def strong (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .strong .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def em (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .em .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def small (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .small .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def span (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .span .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def i (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .i .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def b (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .b .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def u (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .u .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def s (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .s .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def mark (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .mark .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def cite (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .cite .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dfn (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .dfn .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def abbr (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .abbr .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def var (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .var .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def samp (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .samp .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def kbd (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .kbd .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def sub (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .sub .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def sup (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .sup .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def bdi (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .bdi .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def bdo (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .bdo .phrasing .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- A ruby annotation: one or more `<rt>`, or else an `<rp>` followed by one or more `<rt>`, each
followed by an `<rp>`. -/
inductive RubyText where
  | rt (first : Node .rubyText) (rest : List (Node .rubyText) := [])
  | parenthesized (opening : Node .rubyParenthesis) (first : Node .rubyText × Node .rubyParenthesis)
      (rest : List (Node .rubyText × Node .rubyParenthesis) := [])

instance : Coe (Node .rubyText) RubyText := ⟨(.rt · [])⟩

/-- Base text and its annotation. A nested `<ruby>` as the base isn't distinguished from
phrasing content, nor is a `<ruby>` that the spec rules out of the base. -/
structure RubySegment where
  base : List (Node .phrasing)
  text : RubyText

abbrev RubySegment.kinds : List (SegK Web.Cat) :=
  [.many (.html .phrasing),
    .alt [.one (.html .rubyText), .many (.html .rubyText)]
      [.one (.html .rubyParenthesis), .one (.html .rubyText), .one (.html .rubyParenthesis),
        .groups [.one (.html .rubyText), .one (.html .rubyParenthesis)]]]

def RubySegment.segs (s : RubySegment) : Segs Web.vocab.sig Attrs RubySegment.kinds :=
  segs![.many (Nodes.ofList s.base), match s.text with
    | .rt first rest => .inl segs![.one first, .many (Nodes.ofList rest)]
    | .parenthesized opening (t, p) rest =>
      .inr segs![.one opening, .one t, .one p,
        .groups (Groups.ofList (rest.map fun (t, p) => segs![.one t, .one p]))]]

def ruby (segment : RubySegment) (more : List RubySegment := []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Web.elem (.html .ruby) (.html .phrasing) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    (Segs.append segment.segs segs![.groups (Groups.ofList (more.map RubySegment.segs))])

def rt (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .rubyText :=
  listElement .rt .rubyText .phrasing children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def rp (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .rubyParenthesis :=
  listElement .rp .rubyParenthesis .phrasing children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def wbr (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  voidElement .wbr .phrasing (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def q (children : List (Node .phrasing)) (attrs : QAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .q .phrasing .phrasing children (combineAttrs (QAttrs.render attrs) rawAttrs)

def time (children : List (Node .phrasing)) (attrs : TimeAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .time .phrasing .phrasing children (combineAttrs (TimeAttrs.render attrs) rawAttrs)

def data (attrs : DataAttrs) (children : List (Node .phrasing))
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .data .phrasing .phrasing children (combineAttrs (DataAttrs.render attrs) rawAttrs)

def ins {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : InsDelAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .ins cat children (combineAttrs (InsDelAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def del {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : InsDelAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .del cat children (combineAttrs (InsDelAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def br (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  voidElement .br .phrasing (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def form (children : List (Node .flow)) (attrs : FormAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .form .flow .flow children (combineAttrs (FormAttrs.render attrs) rawAttrs)

def fieldset (children : List (Node .flow)) (attrs : FieldsetAttrs := {})
    (rawAttrs : List (String × String) := []) (legend : Option (Node .legend) := none) :
    Node .flow :=
  Web.elem (.html .fieldset) (.html .flow) (combineAttrs (FieldsetAttrs.render attrs) rawAttrs)
    segs![Seg.ofOption _ legend, .many (Nodes.ofList children)]

def legend (children : List (Node .phrasingOrHeading)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .legend :=
  listElement .legend .legend .phrasingOrHeading children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def input (attrs : InputAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  voidElement .input .phrasing (combineAttrs (InputAttrs.render attrs) rawAttrs)

def label (children : List (Node .phrasing)) (attrs : LabelAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .label .phrasing .phrasing children (combineAttrs (LabelAttrs.render attrs) rawAttrs)

def textarea (content : String) (attrs : TextareaAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  textElement .textarea .phrasing content (combineAttrs (TextareaAttrs.render attrs) rawAttrs)

def option (label : String) (attrs : OptionAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .option :=
  textElement .option .option label (combineAttrs (OptionAttrs.render attrs) rawAttrs)

def select (children : List (Node .selectChild)) (attrs : SelectAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .select .phrasing .selectChild children
    (combineAttrs (SelectAttrs.render attrs) rawAttrs)

def datalist (children : List (Node .optionOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .datalist .phrasing .optionOrScript children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def optgroup (attrs : OptgroupAttrs) (children : List (Node .optionOrScript))
    (rawAttrs : List (String × String) := []) : Node .selectChild :=
  listElement .optgroup .selectChild .optionOrScript children
    (combineAttrs (OptgroupAttrs.render attrs) rawAttrs)

def button (children : List (Node .phrasing)) (attrs : ButtonAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .button .phrasing .phrasing children (combineAttrs (ButtonAttrs.render attrs) rawAttrs)

def output (children : List (Node .phrasing)) (attrs : OutputAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .output .phrasing .phrasing children (combineAttrs (OutputAttrs.render attrs) rawAttrs)

def progress (children : List (Node .phrasing)) (attrs : ProgressAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .progress .phrasing .phrasing children
    (combineAttrs (ProgressAttrs.render attrs) rawAttrs)

def meter (children : List (Node .phrasing)) (attrs : MeterAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  listElement .meter .phrasing .phrasing children (combineAttrs (MeterAttrs.render attrs) rawAttrs)

def details (summary : Node .summary) (children : List (Node .flow) := []) (attrs : OpenAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Web.elem (.html .details) (.html .flow) (combineAttrs (OpenAttrs.render attrs) rawAttrs)
    segs![.one summary, .many (Nodes.ofList children)]

def summary (children : List (Node .phrasingOrHeading)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .summary :=
  listElement .summary .summary .phrasingOrHeading children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dialog (children : List (Node .flow)) (attrs : OpenAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  listElement .dialog .flow .flow children (combineAttrs (OpenAttrs.render attrs) rawAttrs)

def img (attrs : ImgAttrs) (rawAttrs : List (String × String) := []) : Node .image :=
  voidElement .img .image (combineAttrs (ImgAttrs.render attrs) rawAttrs)

def hr (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  voidElement .hr .flow (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def picture (sources : List (Node .pictureSourceOrScript)) (img : Node .image)
    (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Web.elem (.html .picture) (.html .phrasing) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    segs![.many (Nodes.ofList sources), .one img]

/-- The attributes a `<source>` in context `cat` takes: `srcset` in a `<picture>`, `src` in a
media element. A media element's is the default, since a list coerced to a `MediaSource` gives
its elements no expected type to choose by. -/
class SourceContext (cat : Category) (attrs : outParam Type) where
  render : attrs → Attrs
  build : Attrs → Node cat

@[default_instance] instance : SourceContext .source SourceAttrs :=
  ⟨SourceAttrs.render, voidElement .source .source⟩
instance : SourceContext .pictureSource PictureSourceAttrs :=
  ⟨PictureSourceAttrs.render, voidElement .source .pictureSource⟩
instance : SourceContext .pictureSourceOrScript PictureSourceAttrs :=
  ⟨PictureSourceAttrs.render, fun attrs => (voidElement .source .pictureSource attrs : Node .pictureSource)⟩

def source {cat : Category} {α : Type} [h : SourceContext cat α] (attrs : α)
    (rawAttrs : List (String × String) := []) : Node cat :=
  h.build (combineAttrs (h.render attrs) rawAttrs)

def track (attrs : TrackAttrs) (rawAttrs : List (String × String) := []) : Node .track :=
  voidElement .track .track (combineAttrs (TrackAttrs.render attrs) rawAttrs)

def iframe (attrs : IframeAttrs) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  voidElement .iframe .phrasing (combineAttrs (IframeAttrs.render attrs) rawAttrs)

def embed (attrs : EmbedAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  voidElement .embed .phrasing (combineAttrs (EmbedAttrs.render attrs) rawAttrs)

def object {cat : Category} [Transparent cat] (attrs : ObjectAttrs := {})
    (children : List (Node cat) := []) (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .object cat children (combineAttrs (ObjectAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

/-- A media element's resource: a `src` attribute, or else any number of `<source>` children. -/
inductive MediaSource where
  | src (url : String)
  | sources (sources : List (Node .source))

instance : Coe (List (Node .source)) MediaSource := ⟨.sources⟩

def MediaSource.attrs : MediaSource → Attrs
  | .src url => reqAttr "src" url
  | .sources _ => []

def MediaSource.children : MediaSource → List (Node .source)
  | .src _ => []
  | .sources ss => ss

/-- A media element: the source, then any number of `<track>`, then transparent fallback
`children`. -/
abbrev mediaElement (e : HtmlModel.Html.Elem) (cat : Category) [Transparent cat]
    (source : MediaSource) (tracks : List (Node .track)) (children : List (Node cat))
    (attrs : Attrs) (hc : Web.Cat.html cat ∈ Web.vocab.cats (.html e))
    (ok : Web.vocab.Admits (.html e) (.html cat)
      [.many (.html .source), .many (.html .track), .many (.html cat)]) : Node cat :=
  Web.elem (.html e) (.html cat) (source.attrs ++ attrs)
    segs![.many (Nodes.ofList source.children), .many (Nodes.ofList tracks),
      .many (Nodes.ofList children)] hc ok

def video {cat : Category} [Transparent cat] (source : MediaSource := .sources [])
    (tracks : List (Node .track) := []) (children : List (Node cat) := [])
    (attrs : VideoAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  mediaElement .video cat source tracks children (combineAttrs (VideoAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def audio {cat : Category} [Transparent cat] (source : MediaSource := .sources [])
    (tracks : List (Node .track) := []) (children : List (Node cat) := [])
    (attrs : AudioAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  mediaElement .audio cat source tracks children (combineAttrs (AudioAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def map {cat : Category} [Transparent cat] (attrs : MapAttrs)
    (children : List (Node cat) := []) (rawAttrs : List (String × String) := []) : Node cat :=
  transparentElement .map cat children (combineAttrs (MapAttrs.render attrs) rawAttrs)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide)
    (by rcases Transparent.cases (cat := cat) with rfl | rfl <;> decide +kernel)

def area (attrs : AreaAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  voidElement .area .phrasing (combineAttrs (AreaAttrs.render attrs) rawAttrs)

/-- A table's body: any number of `<tbody>`, or else bare `<tr>` rows. -/
inductive TableBody where
  | sections (sections : List (Node .tableBodyOrScript))
  | rows (rows : List (Node .tableRowOrScript))

instance : Coe (List (Node .tableBodyOrScript)) TableBody := ⟨.sections⟩
instance : Coe (List (Node .tableRowOrScript)) TableBody := ⟨.rows⟩
instance : Coe (List (Node .tableBody)) TableBody :=
  ⟨fun ss => .sections (ss.map fun (s : Node .tableBody) => (s : Node .tableBodyOrScript))⟩
instance : Coe (List (Node .tableRow)) TableBody :=
  ⟨fun rs => .rows (rs.map fun (r : Node .tableRow) => (r : Node .tableRowOrScript))⟩

/-- An optional caption, any number of column groups, an optional head, the body, and an
optional foot, in that order. The spec wants at least one row where `body` is `.rows`, but
`.rows []` renders as `.sections []` does, which is valid. -/
def table (body : TableBody := .sections []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) (caption : Option (Node .tableCaption) := none)
    (colgroups : List (Node .tableColumnGroupOrScript) := [])
    (thead : Option (Node .tableHead) := none) (tfoot : Option (Node .tableFoot) := none) :
    Node .flow :=
  let body : Seg Web.vocab.sig Attrs
      (.alt [.many (.html .tableBodyOrScript)] [.many (.html .tableRowOrScript)]) :=
    match body with
    | .sections ss => .inl segs![.many (Nodes.ofList ss)]
    | .rows rs => .inr segs![.many (Nodes.ofList rs)]
  Web.elem (.html .table) (.html .flow) (combineAttrs (HtmlAttrs.render attrs) rawAttrs)
    segs![Seg.ofOption _ caption, .many (Nodes.ofList colgroups), Seg.ofOption _ thead, body,
      Seg.ofOption _ tfoot]

def caption (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCaption :=
  listElement .caption .tableCaption .flow children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- A `<colgroup>` holds either a `span` attribute or `<col>` children, never both. -/
inductive ColgroupContent where
  | span (span : String)
  | cols (cols : List (Node .tableColumnOrTemplate))

instance : Coe (List (Node .tableColumnOrTemplate)) ColgroupContent := ⟨.cols⟩
instance : Coe (List (Node .tableColumn)) ColgroupContent :=
  ⟨fun cs => .cols (cs.map fun (c : Node .tableColumn) => (c : Node .tableColumnOrTemplate))⟩

def ColgroupContent.attrs : ColgroupContent → Attrs
  | .span n => reqAttr "span" n
  | .cols _ => []

def ColgroupContent.children : ColgroupContent → List (Node .tableColumnOrTemplate)
  | .span _ => []
  | .cols cs => cs

def colgroup (content : ColgroupContent := .cols []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableColumnGroup :=
  listElement .colgroup .tableColumnGroup .tableColumnOrTemplate content.children
    (combineAttrs (content.attrs ++ HtmlAttrs.render attrs) rawAttrs)

def col (attrs : ColAttrs := {}) (rawAttrs : List (String × String) := []) : Node .tableColumn :=
  voidElement .col .tableColumn (combineAttrs (ColAttrs.render attrs) rawAttrs)

def thead (children : List (Node .tableRowOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableHead :=
  listElement .thead .tableHead .tableRowOrScript children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tbody (children : List (Node .tableRowOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableBody :=
  listElement .tbody .tableBody .tableRowOrScript children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tfoot (children : List (Node .tableRowOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableFoot :=
  listElement .tfoot .tableFoot .tableRowOrScript children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tr (children : List (Node .tableCellOrScript)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableRow :=
  listElement .tr .tableRow .tableCellOrScript children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def th (children : List (Node .flow)) (attrs : ThAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCell :=
  listElement .th .tableCell .flow children (combineAttrs (ThAttrs.render attrs) rawAttrs)

def td (children : List (Node .flow)) (attrs : TdAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCell :=
  listElement .td .tableCell .flow children (combineAttrs (TdAttrs.render attrs) rawAttrs)

end Html

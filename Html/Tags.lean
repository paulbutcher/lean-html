/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Node
public import Html.Escape
public import Html.Attrs

/-!
Named tag functions, built on `Html/Node.lean`'s constructor shapes and
`Html/Attrs.lean`'s typed attribute vocabulary.

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
- Script-supporting elements (`script`, `template`) may intermix with the children of a table,
  list, `dl`, `select`, `picture`, `hgroup`, or `ruby`; that isn't modelled, so there they are
  rejected.
-/

@[expose] public section

namespace Html

def combineAttrs (specific : Attrs) (rawAttrs : List (String × String)) : Attrs :=
  specific ++ rawAttrs.map fun (n, v) => .value n v

-- Structure: flow content, flow children.
def div (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "div" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def section_ (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "section" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def article (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "article" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def header (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "header" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def footer (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "footer" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def nav (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "nav" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def aside (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "aside" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Any number of `<p>`, then one heading, then any number of `<p>`. -/
def hgroup (before : List (Node .paragraph)) (heading : Node .heading)
    (after : List (Node .paragraph) := []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementSeq .flow "hgroup" (before.map Node.child ++ heading.child :: after.map Node.child)
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def address (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "address" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def main (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "main" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def search (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "search" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Exactly one `<title>` and at most one `<base>`. `base` renders first, so that it precedes
every URL it resolves; `title` renders last, so that a long one can't push a
`<meta charset>` among `children` out of the first 1024 bytes. -/
def head (title : Node .title) (children : List (Node .metadata) := []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) (base : Option (Node .base) := none) : Node .head :=
  Node.elementSeq .head "head" (base.toList.map Node.child ++ children.map Node.child ++ [title.child])
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def body (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .body :=
  Node.elementOf .body .flow "body" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def title (content : String) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .title :=
  Node.textElement .title "title" content (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Takes `rawAttrs` as its primary content rather than a typed
attrs record, since a meta tag's shape varies by purpose:
`[("charset", "utf-8")]`, `[("name", "viewport"), ("content", "...")]`,
`[("http-equiv", "..."), ("content", "...")]`, and so on, with no one
shape common enough to single out as required fields. -/
def meta_ {cat : Category} [Metadata cat] (rawAttrs : List (String × String))
    (attrs : HtmlAttrs := {}) : Node cat :=
  Node.voidElement cat "meta" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def link {cat : Category} [Metadata cat] (attrs : LinkAttrs)
    (rawAttrs : List (String × String) := []) : Node cat :=
  Node.voidElement cat "link" (combineAttrs (LinkAttrs.render attrs) rawAttrs)

def base (attrs : BaseAttrs := {}) (rawAttrs : List (String × String) := []) : Node .base :=
  Node.voidElement .base "base" (combineAttrs (BaseAttrs.render attrs) rawAttrs)

def script {cat : Category} [Metadata cat] (attrs : ScriptAttrs)
    (rawAttrs : List (String × String) := []) : Node cat :=
  Node.element cat "script" [] (combineAttrs (ScriptAttrs.render attrs) rawAttrs)

def noscript {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "noscript" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def template {cat : Category} [Metadata cat] (children : List (Node .flow))
    (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.elementOf cat .flow "template" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def canvas {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : CanvasAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "canvas" children (combineAttrs (CanvasAttrs.render attrs) rawAttrs)

def slot {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : SlotAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "slot" children (combineAttrs (SlotAttrs.render attrs) rawAttrs)

def p (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .paragraph :=
  Node.elementOf .paragraph .phrasing "p" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h1 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  Node.elementOf .heading .phrasing "h1" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h2 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  Node.elementOf .heading .phrasing "h2" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h3 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  Node.elementOf .heading .phrasing "h3" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h4 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  Node.elementOf .heading .phrasing "h4" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h5 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  Node.elementOf .heading .phrasing "h5" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h6 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .heading :=
  Node.elementOf .heading .phrasing "h6" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ul (children : List (Node .listItem)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .listItem "ul" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ol (children : List (Node .listItem)) (attrs : OlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .listItem "ol" children (combineAttrs (OlAttrs.render attrs) rawAttrs)

def li (children : List (Node .flow)) (attrs : LiAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .listItem :=
  Node.elementOf .listItem .flow "li" children (combineAttrs (LiAttrs.render attrs) rawAttrs)

def menu (children : List (Node .listItem)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .listItem "menu" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- One or more `<dt>` followed by one or more `<dd>`. -/
structure DlGroup where
  term : Node .term
  moreTerms : List (Node .term) := []
  desc : Node .description
  moreDescs : List (Node .description) := []

def DlGroup.children (g : DlGroup) : List Child :=
  g.term.child :: g.moreTerms.map Node.child ++ g.desc.child :: g.moreDescs.map Node.child

/-- A `<dl>` holds either bare groups or groups each wrapped in a `<div>`, never a mixture. -/
inductive DlContent where
  | groups (groups : List DlGroup)
  | divs (divs : List (Node .descriptionGroup))

instance : Coe (List DlGroup) DlContent := ⟨.groups⟩
instance : Coe (List (Node .descriptionGroup)) DlContent := ⟨.divs⟩

def DlContent.children : DlContent → List Child
  | .groups gs => gs.flatMap DlGroup.children
  | .divs ds => ds.map Node.child

def dl (content : DlContent := .groups []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementSeq .flow "dl" content.children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- A `<div>` wrapping one group of a `<dl>`. -/
def dlDiv (group : DlGroup) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .descriptionGroup :=
  Node.elementSeq .descriptionGroup "div" group.children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dt (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .term :=
  Node.elementOf .term .flow "dt" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dd (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .description :=
  Node.elementOf .description .flow "dd" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def blockquote (children : List (Node .flow)) (attrs : BlockquoteAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "blockquote" children (combineAttrs (BlockquoteAttrs.render attrs) rawAttrs)

/-- Where a `<figure>`'s caption goes, if it has one: its first child or its last. -/
inductive FigureCaption where
  | none
  | first (caption : Node .figureCaption)
  | last (caption : Node .figureCaption)

def figure (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) (caption : FigureCaption := .none) : Node .flow :=
  let children := children.map Node.child
  Node.elementSeq .flow "figure"
    (match caption with
      | .none => children
      | .first c => c.child :: children
      | .last c => children ++ [c.child])
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def figcaption (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .figureCaption :=
  Node.elementOf .figureCaption .flow "figcaption" children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

-- `pre`: flow content, phrasing-only children (preformatted text).
def pre (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "pre" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def code (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "code" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def a {cat : Category} [Transparent cat] (attrs : AAttrs) (children : List (Node cat))
    (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "a" children (combineAttrs (AAttrs.render attrs) rawAttrs)

def strong (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "strong" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def em (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "em" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def small (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "small" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def span (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "span" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def i (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "i" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def b (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "b" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def u (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "u" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def s (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "s" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def mark (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "mark" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def cite (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "cite" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dfn (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "dfn" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def abbr (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "abbr" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def var (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "var" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def samp (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "samp" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def kbd (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "kbd" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def sub (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "sub" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def sup (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "sup" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def bdi (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "bdi" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def bdo (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "bdo" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- A ruby annotation: one or more `<rt>`, or else an `<rp>` followed by one or more `<rt>`, each
followed by an `<rp>`. -/
inductive RubyText where
  | rt (first : Node .rubyText) (rest : List (Node .rubyText) := [])
  | parenthesized (opening : Node .rubyParenthesis) (first : Node .rubyText × Node .rubyParenthesis)
      (rest : List (Node .rubyText × Node .rubyParenthesis) := [])

instance : Coe (Node .rubyText) RubyText := ⟨(.rt · [])⟩

def RubyText.children : RubyText → List Child
  | .rt first rest => first.child :: rest.map Node.child
  | .parenthesized opening first rest =>
    opening.child :: (first :: rest).flatMap fun (t, p) => [t.child, p.child]

/-- Base text and its annotation. A nested `<ruby>` as the base isn't distinguished from
phrasing content, nor is a `<ruby>` that the spec rules out of the base. -/
structure RubySegment where
  base : List (Node .phrasing)
  text : RubyText

def RubySegment.children (s : RubySegment) : List Child :=
  s.base.map Node.child ++ s.text.children

def ruby (segment : RubySegment) (more : List RubySegment := []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.elementSeq .phrasing "ruby" ((segment :: more).flatMap RubySegment.children)
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def rt (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .rubyText :=
  Node.elementOf .rubyText .phrasing "rt" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def rp (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .rubyParenthesis :=
  Node.elementOf .rubyParenthesis .phrasing "rp" children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def wbr (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "wbr" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def q (children : List (Node .phrasing)) (attrs : QAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "q" children (combineAttrs (QAttrs.render attrs) rawAttrs)

def time (children : List (Node .phrasing)) (attrs : TimeAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "time" children (combineAttrs (TimeAttrs.render attrs) rawAttrs)

def data (attrs : DataAttrs) (children : List (Node .phrasing))
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "data" children (combineAttrs (DataAttrs.render attrs) rawAttrs)

def ins {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : InsDelAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "ins" children (combineAttrs (InsDelAttrs.render attrs) rawAttrs)

def del {cat : Category} [Transparent cat] (children : List (Node cat))
    (attrs : InsDelAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "del" children (combineAttrs (InsDelAttrs.render attrs) rawAttrs)

def br (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "br" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def form (children : List (Node .flow)) (attrs : FormAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "form" children (combineAttrs (FormAttrs.render attrs) rawAttrs)

def fieldset (children : List (Node .flow)) (attrs : FieldsetAttrs := {})
    (rawAttrs : List (String × String) := []) (legend : Option (Node .legend) := none) :
    Node .flow :=
  Node.elementSeq .flow "fieldset" (legend.toList.map Node.child ++ children.map Node.child)
    (combineAttrs (FieldsetAttrs.render attrs) rawAttrs)

/-- Heading content, which the spec also admits here, is rejected. -/
def legend (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .legend :=
  Node.elementOf .legend .phrasing "legend" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def input (attrs : InputAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "input" (combineAttrs (InputAttrs.render attrs) rawAttrs)

def label (children : List (Node .phrasing)) (attrs : LabelAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "label" children (combineAttrs (LabelAttrs.render attrs) rawAttrs)

def textarea (content : String) (attrs : TextareaAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.textElement .phrasing "textarea" content (combineAttrs (TextareaAttrs.render attrs) rawAttrs)

def option (label : String) (attrs : OptionAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .option :=
  Node.textElement .option "option" label (combineAttrs (OptionAttrs.render attrs) rawAttrs)

def select (children : List (Node .selectChild)) (attrs : SelectAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.elementOf .phrasing .selectChild "select" children
    (combineAttrs (SelectAttrs.render attrs) rawAttrs)

def datalist (children : List (Node .option)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.elementOf .phrasing .option "datalist" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def optgroup (attrs : OptgroupAttrs) (children : List (Node .option))
    (rawAttrs : List (String × String) := []) : Node .selectChild :=
  Node.elementOf .selectChild .option "optgroup" children
    (combineAttrs (OptgroupAttrs.render attrs) rawAttrs)

def button (children : List (Node .phrasing)) (attrs : ButtonAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "button" children (combineAttrs (ButtonAttrs.render attrs) rawAttrs)

def output (children : List (Node .phrasing)) (attrs : OutputAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "output" children (combineAttrs (OutputAttrs.render attrs) rawAttrs)

def progress (children : List (Node .phrasing)) (attrs : ProgressAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "progress" children (combineAttrs (ProgressAttrs.render attrs) rawAttrs)

def meter (children : List (Node .phrasing)) (attrs : MeterAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "meter" children (combineAttrs (MeterAttrs.render attrs) rawAttrs)

def details (summary : Node .summary) (children : List (Node .flow) := []) (attrs : OpenAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementSeq .flow "details" (summary.child :: children.map Node.child)
    (combineAttrs (OpenAttrs.render attrs) rawAttrs)

/-- Heading content, which the spec also admits here, is rejected. -/
def summary (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .summary :=
  Node.elementOf .summary .phrasing "summary" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dialog (children : List (Node .flow)) (attrs : OpenAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "dialog" children (combineAttrs (OpenAttrs.render attrs) rawAttrs)

def img (attrs : ImgAttrs) (rawAttrs : List (String × String) := []) : Node .image :=
  Node.voidElement .image "img" (combineAttrs (ImgAttrs.render attrs) rawAttrs)

def hr (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "hr" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def picture (sources : List (Node .source)) (img : Node .image) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.elementSeq .phrasing "picture" (sources.map Node.child ++ [img.child])
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def source (attrs : SourceAttrs := {}) (rawAttrs : List (String × String) := []) : Node .source :=
  Node.voidElement .source "source" (combineAttrs (SourceAttrs.render attrs) rawAttrs)

def track (attrs : TrackAttrs) (rawAttrs : List (String × String) := []) : Node .track :=
  Node.voidElement .track "track" (combineAttrs (TrackAttrs.render attrs) rawAttrs)

def iframe (attrs : IframeAttrs) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "iframe" [] (combineAttrs (IframeAttrs.render attrs) rawAttrs)

def embed (attrs : EmbedAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "embed" (combineAttrs (EmbedAttrs.render attrs) rawAttrs)

def object {cat : Category} [Transparent cat] (attrs : ObjectAttrs := {})
    (children : List (Node cat) := []) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "object" children (combineAttrs (ObjectAttrs.render attrs) rawAttrs)

/-- A media element's resource: a `src` attribute, or else any number of `<source>` children. -/
inductive MediaSource where
  | src (url : String)
  | sources (sources : List (Node .source))

instance : Coe (List (Node .source)) MediaSource := ⟨.sources⟩

def MediaSource.attrs : MediaSource → Attrs
  | .src url => reqAttr "src" url
  | .sources _ => []

def MediaSource.children : MediaSource → List Child
  | .src _ => []
  | .sources ss => ss.map Node.child

/-- The source, then any number of `<track>`, then transparent fallback `children`. -/
def video {cat : Category} [Transparent cat] (source : MediaSource := .sources [])
    (tracks : List (Node .track) := []) (children : List (Node cat) := [])
    (attrs : VideoAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "video" children
    (combineAttrs (source.attrs ++ VideoAttrs.render attrs) rawAttrs)
    (source.children ++ tracks.map Node.child)

/-- The source, then any number of `<track>`, then transparent fallback `children`. -/
def audio {cat : Category} [Transparent cat] (source : MediaSource := .sources [])
    (tracks : List (Node .track) := []) (children : List (Node cat) := [])
    (attrs : AudioAttrs := {}) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "audio" children
    (combineAttrs (source.attrs ++ AudioAttrs.render attrs) rawAttrs)
    (source.children ++ tracks.map Node.child)

def map {cat : Category} [Transparent cat] (attrs : MapAttrs)
    (children : List (Node cat) := []) (rawAttrs : List (String × String) := []) : Node cat :=
  Node.transparentElement cat "map" children (combineAttrs (MapAttrs.render attrs) rawAttrs)

def area (attrs : AreaAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "area" (combineAttrs (AreaAttrs.render attrs) rawAttrs)

/-- A table's body: any number of `<tbody>`, or else bare `<tr>` rows. -/
inductive TableBody where
  | sections (sections : List (Node .tableBody))
  | rows (rows : List (Node .tableRow))

instance : Coe (List (Node .tableBody)) TableBody := ⟨.sections⟩
instance : Coe (List (Node .tableRow)) TableBody := ⟨.rows⟩

def TableBody.children : TableBody → List Child
  | .sections ss => ss.map Node.child
  | .rows rs => rs.map Node.child

/-- An optional caption, any number of column groups, an optional head, the body, and an
optional foot, in that order. The spec wants at least one row where `body` is `.rows`, but
`.rows []` renders as `.sections []` does, which is valid. -/
def table (body : TableBody := .sections []) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) (caption : Option (Node .tableCaption) := none)
    (colgroups : List (Node .tableColumnGroup) := []) (thead : Option (Node .tableHead) := none)
    (tfoot : Option (Node .tableFoot) := none) : Node .flow :=
  Node.elementSeq .flow "table"
    (caption.toList.map Node.child ++ colgroups.map Node.child ++ thead.toList.map Node.child ++
      body.children ++ tfoot.toList.map Node.child)
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def caption (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCaption :=
  Node.elementOf .tableCaption .flow "caption" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def colgroup (children : List (Node .tableColumn)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableColumnGroup :=
  Node.elementOf .tableColumnGroup .tableColumn "colgroup" children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def col (attrs : ColAttrs := {}) (rawAttrs : List (String × String) := []) : Node .tableColumn :=
  Node.voidElement .tableColumn "col" (combineAttrs (ColAttrs.render attrs) rawAttrs)

def thead (children : List (Node .tableRow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableHead :=
  Node.elementOf .tableHead .tableRow "thead" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tbody (children : List (Node .tableRow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableBody :=
  Node.elementOf .tableBody .tableRow "tbody" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tfoot (children : List (Node .tableRow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableFoot :=
  Node.elementOf .tableFoot .tableRow "tfoot" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tr (children : List (Node .tableCell)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableRow :=
  Node.elementOf .tableRow .tableCell "tr" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def th (children : List (Node .flow)) (attrs : ThAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCell :=
  Node.elementOf .tableCell .flow "th" children (combineAttrs (ThAttrs.render attrs) rawAttrs)

def td (children : List (Node .flow)) (attrs : TdAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCell :=
  Node.elementOf .tableCell .flow "td" children (combineAttrs (TdAttrs.render attrs) rawAttrs)

/-! `WellFormed` for the tag functions built on `Node.elementSeq`, whose children aren't a
single list. Each takes one hypothesis per argument; `Node.WellFormedChildren`'s lemmas discharge
those for a `children` function. -/

namespace Node

open WellFormedChildren

theorem hgroup_wellFormed (before : List (Node .paragraph)) (heading : Node .heading)
    (after : List (Node .paragraph)) (attrs : HtmlAttrs) (rawAttrs : List (String × String))
    (hbefore : ∀ n ∈ before, WellFormed n) (hheading : WellFormed heading)
    (hafter : ∀ n ∈ after, WellFormed n) :
    WellFormed (hgroup before heading after attrs rawAttrs) :=
  elementSeq_wellFormed _ _ _ _ ((ofNodes hbefore).append (.cons hheading (ofNodes hafter)))

theorem head_wellFormed (title : Node .title) (children : List (Node .metadata))
    (attrs : HtmlAttrs) (rawAttrs : List (String × String)) (base : Option (Node .base))
    (htitle : WellFormed title) (hchildren : ∀ n ∈ children, WellFormed n)
    (hbase : ∀ n ∈ base, WellFormed n) :
    WellFormed (head title children attrs rawAttrs base) :=
  elementSeq_wellFormed _ _ _ _
    (((ofOption hbase).append (ofNodes hchildren)).append (.cons htitle .nil))

theorem DlGroup.children_wellFormed (g : DlGroup) (hterm : WellFormed g.term)
    (hmoreTerms : ∀ n ∈ g.moreTerms, WellFormed n) (hdesc : WellFormed g.desc)
    (hmoreDescs : ∀ n ∈ g.moreDescs, WellFormed n) : WellFormedChildren g.children :=
  .cons hterm ((ofNodes hmoreTerms).append (.cons hdesc (ofNodes hmoreDescs)))

theorem dl_wellFormed (content : DlContent) (attrs : HtmlAttrs)
    (rawAttrs : List (String × String)) (hcontent : WellFormedChildren content.children) :
    WellFormed (dl content attrs rawAttrs) :=
  elementSeq_wellFormed _ _ _ _ hcontent

theorem dlDiv_wellFormed (group : DlGroup) (attrs : HtmlAttrs)
    (rawAttrs : List (String × String)) (hgroup : WellFormedChildren group.children) :
    WellFormed (dlDiv group attrs rawAttrs) :=
  elementSeq_wellFormed _ _ _ _ hgroup

theorem figure_wellFormed (children : List (Node .flow)) (attrs : HtmlAttrs)
    (rawAttrs : List (String × String)) (caption : FigureCaption)
    (hchildren : ∀ n ∈ children, WellFormed n)
    (hcaption : ∀ c, caption = .first c ∨ caption = .last c → WellFormed c) :
    WellFormed (figure children attrs rawAttrs caption) := by
  apply elementSeq_wellFormed
  cases caption with
  | none => exact ofNodes hchildren
  | first c => exact .cons (hcaption c (.inl rfl)) (ofNodes hchildren)
  | last c => exact (ofNodes hchildren).append (.cons (hcaption c (.inr rfl)) .nil)

theorem ruby_wellFormed (segment : RubySegment) (more : List RubySegment) (attrs : HtmlAttrs)
    (rawAttrs : List (String × String))
    (hsegments : ∀ s ∈ segment :: more, WellFormedChildren s.children) :
    WellFormed (ruby segment more attrs rawAttrs) :=
  elementSeq_wellFormed _ _ _ _ (flatMap hsegments)

theorem fieldset_wellFormed (children : List (Node .flow)) (attrs : FieldsetAttrs)
    (rawAttrs : List (String × String)) (legend : Option (Node .legend))
    (hchildren : ∀ n ∈ children, WellFormed n) (hlegend : ∀ n ∈ legend, WellFormed n) :
    WellFormed (fieldset children attrs rawAttrs legend) :=
  elementSeq_wellFormed _ _ _ _ ((ofOption hlegend).append (ofNodes hchildren))

theorem details_wellFormed (summary : Node .summary) (children : List (Node .flow))
    (attrs : OpenAttrs) (rawAttrs : List (String × String)) (hsummary : WellFormed summary)
    (hchildren : ∀ n ∈ children, WellFormed n) :
    WellFormed (details summary children attrs rawAttrs) :=
  elementSeq_wellFormed _ _ _ _ (.cons hsummary (ofNodes hchildren))

theorem picture_wellFormed (sources : List (Node .source)) (img : Node .image)
    (attrs : HtmlAttrs) (rawAttrs : List (String × String))
    (hsources : ∀ n ∈ sources, WellFormed n) (himg : WellFormed img) :
    WellFormed (picture sources img attrs rawAttrs) :=
  elementSeq_wellFormed _ _ _ _ ((ofNodes hsources).append (.cons himg .nil))

theorem video_wellFormed {cat : Category} [Transparent cat] (source : MediaSource)
    (tracks : List (Node .track)) (children : List (Node cat)) (attrs : VideoAttrs)
    (rawAttrs : List (String × String)) (hsource : WellFormedChildren source.children)
    (htracks : ∀ n ∈ tracks, WellFormed n) (hchildren : ∀ n ∈ children, WellFormed n) :
    WellFormed (video source tracks children attrs rawAttrs) :=
  transparentElement_wellFormed _ _ _ _ _ (hsource.append (ofNodes htracks)) hchildren

theorem audio_wellFormed {cat : Category} [Transparent cat] (source : MediaSource)
    (tracks : List (Node .track)) (children : List (Node cat)) (attrs : AudioAttrs)
    (rawAttrs : List (String × String)) (hsource : WellFormedChildren source.children)
    (htracks : ∀ n ∈ tracks, WellFormed n) (hchildren : ∀ n ∈ children, WellFormed n) :
    WellFormed (audio source tracks children attrs rawAttrs) :=
  transparentElement_wellFormed _ _ _ _ _ (hsource.append (ofNodes htracks)) hchildren

theorem table_wellFormed (body : TableBody) (attrs : HtmlAttrs)
    (rawAttrs : List (String × String)) (caption : Option (Node .tableCaption))
    (colgroups : List (Node .tableColumnGroup)) (thead : Option (Node .tableHead))
    (tfoot : Option (Node .tableFoot)) (hbody : WellFormedChildren body.children)
    (hcaption : ∀ n ∈ caption, WellFormed n) (hcolgroups : ∀ n ∈ colgroups, WellFormed n)
    (hthead : ∀ n ∈ thead, WellFormed n) (htfoot : ∀ n ∈ tfoot, WellFormed n) :
    WellFormed (table body attrs rawAttrs caption colgroups thead tfoot) :=
  elementSeq_wellFormed _ _ _ _
    (((((ofOption hcaption).append (ofNodes hcolgroups)).append (ofOption hthead)).append hbody).append
      (ofOption htfoot))

end Node

end Html

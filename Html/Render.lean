/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Node

@[expose] public section

namespace Html

open HtmlModel

/-- The HTML elements with no closing tag. -/
def isVoid : Web.Elem → Bool
  | .html .area | .html .base | .html .br | .html .col | .html .embed | .html .hr | .html .img
  | .html .input | .html .link | .html .meta | .html .source | .html .track | .html .wbr => true
  | _ => false

def voidTag (dialect : Dialect) (name : String) (attrs : Attrs) : String :=
  match dialect with
  | .html5 => s!"<{name}{Attrs.render attrs dialect}>"
  | .xhtml => s!"<{name}{Attrs.render attrs dialect} />"

def openTag (dialect : Dialect) (name : String) (attrs : Attrs) : String :=
  s!"<{name}{Attrs.render attrs dialect}>"

mutual
/-- `lineBreaks` puts each child that isn't phrasing content on a line of its own within a
parent that has any such child, beginning a line only where one hasn't already begun, which is
the convention cmark follows. -/
def renderInto (dialect : Dialect) (lineBreaks : Bool) : Tree → String → String
  | .text s, acc => acc ++ escape s
  | .verbatim _ s, acc => acc ++ s
  | .elem e c attrs children, acc =>
    if isVoid e then acc ++ voidTag dialect (Web.vocab.name e) attrs
    else
      let parent : Tree := .elem e c attrs children
      renderChildren dialect lineBreaks parent (!parent.childrenInline) children
        (acc ++ openTag dialect (Web.vocab.name e) attrs) ++ s!"</{Web.vocab.name e}>"
  | .unchecked ns name c attrs children, acc =>
    let parent : Tree := .unchecked ns name c attrs children
    renderChildren dialect lineBreaks parent (!parent.childrenInline) children
      (acc ++ openTag dialect name attrs) ++ s!"</{name}>"

def renderChildren (dialect : Dialect) (lineBreaks : Bool) (parent : Tree) (block : Bool) :
    List Tree → String → String
  | [], acc => acc
  | t :: ts, acc =>
    renderChildren dialect lineBreaks parent block ts
      (if lineBreaks && block && !parent.isInlineIn t then
        renderInto dialect lineBreaks t (if acc.endsWith "\n" then acc else acc ++ "\n") ++ "\n"
      else renderInto dialect lineBreaks t acc)
end

def indent (depth : Nat) (unit : String) : String :=
  String.join (List.replicate depth unit)

def closePretty (unit : String) (block : Bool) (depth : Nat) (name acc : String) : String :=
  (if block then acc ++ "\n" ++ indent depth unit else acc) ++ s!"</{name}>"

mutual
/-- Inline layout if every child is phrasing content, else one line per child, except that
adjacent phrasing children share a line: whitespace between phrasing siblings renders, so the
pretty-printer must never put any there. -/
def renderPrettyInto (unit : String) (dialect : Dialect) : Tree → Nat → String → String
  | .text s, _, acc => acc ++ escape s
  | .verbatim _ s, _, acc => acc ++ s
  | .elem e c attrs children, depth, acc =>
    if isVoid e then acc ++ voidTag dialect (Web.vocab.name e) attrs
    else
      let parent : Tree := .elem e c attrs children
      let block := !parent.childrenInline
      closePretty unit block depth (Web.vocab.name e)
        (renderPrettyChildren unit dialect parent block depth false children
          (acc ++ openTag dialect (Web.vocab.name e) attrs))
  | .unchecked ns name c attrs children, depth, acc =>
    let parent : Tree := .unchecked ns name c attrs children
    let block := !parent.childrenInline
    closePretty unit block depth name
      (renderPrettyChildren unit dialect parent block depth false children
        (acc ++ openTag dialect name attrs))

/-- `prevInline` says whether the child before was phrasing content, so that a run of phrasing
children stays on one line. -/
def renderPrettyChildren (unit : String) (dialect : Dialect) (parent : Tree) (block : Bool)
    (depth : Nat) (prevInline : Bool) : List Tree → String → String
  | [], acc => acc
  | t :: ts, acc =>
    let inline := parent.isInlineIn t
    let acc := if block && !(prevInline && inline) then acc ++ "\n" ++ indent (depth + 1) unit
      else acc
    renderPrettyChildren unit dialect parent block depth inline ts
      (renderPrettyInto unit dialect t (if block then depth + 1 else depth) acc)
end

namespace Node

/-- Render a node as compact HTML. With `lineBreaks`, each child that isn't phrasing content
starts a line and ends one, as in cmark's output; the node itself gets no line break around
it. -/
def render {cat : Category} (n : Node cat) (dialect : Dialect := .html5)
    (lineBreaks : Bool := false) : String :=
  renderInto dialect lineBreaks n.erase ""

/-- Render a node as indented, human-readable HTML.
`unit` is the string repeated per indentation level (default two spaces). Line breaks go only
beside a child that isn't phrasing content, since between phrasing siblings they would
render. -/
def renderPretty {cat : Category} (n : Node cat) (unit : String := "  ")
    (dialect : Dialect := .html5) : String :=
  renderPrettyInto unit dialect n.erase 0 ""

/-- A text node renders to exactly its escaped content, with nothing else spliced in. -/
theorem render_text (s : String) (dialect : Dialect := .html5) (lineBreaks : Bool := false) :
    (text s).render dialect lineBreaks = escape s := by
  simp [render, text, renderInto, HtmlModel.Node.erase]

end Node

/-- The well-formed-markup grammar that `render` is proved to produce, given no `unsafeRaw` use
(`Node.render_wellFormed`). A string is well-formed if every `<` it contains opens a tag
immediately matched, once its contents are exhausted, by a literal `</tag>`, or else opens a
void tag: unclosed under `.html5`, self-closing under `.xhtml`. Every attribute run is a
`WellFormedAttrs` for the same dialect, and nowhere else does a `<` or `>` appear at all. This
is a spec of the *output string*, stated independently of `Tree`; at `.xhtml` it's the XML
claim, every tag closed and every attribute given a value. -/
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

/-- What rendering appends to the accumulator is itself well-formed. The accumulator matters
only through `lineBreaks`, which inspects its last character, so this is stated for every
accumulator rather than as rendering from `""` and prepending. -/
private def Appends (dialect : Dialect) (f : String → String) : Prop :=
  ∀ acc, ∃ y, f acc = acc ++ y ∧ WellFormedHtml dialect y

private theorem empty_wellFormed (dialect : Dialect) : WellFormedHtml dialect "" :=
  .text (by simp)

private theorem newline_wellFormed (dialect : Dialect) : WellFormedHtml dialect "\n" :=
  .text (by simp)

private theorem escape_safe' (s : String) : ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' :=
  fun c hc => ⟨(escape_safe s c hc).1, (escape_safe s c hc).2.1⟩

private theorem voidTag_wellFormed (dialect : Dialect) (name : String) (attrs : Attrs) :
    WellFormedHtml dialect (voidTag dialect name attrs) := by
  cases dialect
  · exact .void (tag := name) (Attrs.render_wellFormed attrs .html5)
  · exact .selfClosing (tag := name) (Attrs.render_wellFormed attrs .xhtml)

/-- An element's children, rendered after its opening tag, close into a well-formed element. -/
private theorem element_appends (dialect : Dialect) (name : String) (attrs : Attrs)
    (body : String → String) (hbody : Appends dialect body) :
    Appends dialect (fun acc => body (acc ++ openTag dialect name attrs) ++ s!"</{name}>") := by
  intro acc
  obtain ⟨y, h, hy⟩ := hbody (acc ++ openTag dialect name attrs)
  refine ⟨openTag dialect name attrs ++ y ++ s!"</{name}>", ?_,
    .elem (Attrs.render_wellFormed attrs dialect) hy⟩
  simp only [h, String.append_assoc]

mutual
private theorem renderInto_appends (dialect : Dialect) (lineBreaks : Bool) :
    (t : Tree) → t.hasVerbatim = false → Appends dialect (renderInto dialect lineBreaks t)
  | .text s, _ => fun acc => ⟨escape s, by simp [renderInto], .text (escape_safe' s)⟩
  | .verbatim _ _, h => by simp [Tree.hasVerbatim] at h
  | .elem e c attrs children, h => by
    have hch : Tree.anyVerbatim children = false := by simpa [Tree.hasVerbatim] using h
    intro acc
    by_cases hv : isVoid e
    · exact ⟨voidTag dialect (Web.vocab.name e) attrs, by simp [renderInto, hv],
        voidTag_wellFormed ..⟩
    · have := element_appends dialect (Web.vocab.name e) attrs _
        (renderChildren_appends dialect lineBreaks (.elem e c attrs children)
          (!(Tree.elem e c attrs children : Tree).childrenInline) children hch) acc
      simpa [renderInto, hv] using this
  | .unchecked ns name c attrs children, h => by
    have hch : Tree.anyVerbatim children = false := by simpa [Tree.hasVerbatim] using h
    intro acc
    have := element_appends dialect name attrs _
      (renderChildren_appends dialect lineBreaks (.unchecked ns name c attrs children)
        (!(Tree.unchecked ns name c attrs children : Tree).childrenInline) children hch) acc
    simpa [renderInto] using this

private theorem renderChildren_appends (dialect : Dialect) (lineBreaks : Bool) (parent : Tree)
    (block : Bool) :
    (ts : List Tree) → Tree.anyVerbatim ts = false →
      Appends dialect (renderChildren dialect lineBreaks parent block ts)
  | [], _ => fun acc => ⟨"", by simp [renderChildren], empty_wellFormed dialect⟩
  | t :: ts, h => by
    simp only [Tree.anyVerbatim, Bool.or_eq_false_iff] at h
    have ht := renderInto_appends dialect lineBreaks t h.1
    have hts := renderChildren_appends dialect lineBreaks parent block ts h.2
    intro acc
    have hstep : ∃ y, (if lineBreaks && block && !parent.isInlineIn t then
        renderInto dialect lineBreaks t (if acc.endsWith "\n" then acc else acc ++ "\n") ++ "\n"
      else renderInto dialect lineBreaks t acc) = acc ++ y ∧ WellFormedHtml dialect y := by
      split
      · split
        · obtain ⟨y, hy, hwf⟩ := ht acc
          exact ⟨y ++ "\n", by rw [hy, String.append_assoc], hwf.append (newline_wellFormed _)⟩
        · obtain ⟨y, hy, hwf⟩ := ht (acc ++ "\n")
          exact ⟨"\n" ++ y ++ "\n", by simp [hy, String.append_assoc],
            ((newline_wellFormed _).append hwf).append (newline_wellFormed _)⟩
      · exact ht acc
    obtain ⟨y₁, h₁, hy₁⟩ := hstep
    obtain ⟨y₂, h₂, hy₂⟩ := hts (acc ++ y₁)
    refine ⟨y₁ ++ y₂, ?_, hy₁.append hy₂⟩
    simp only [renderChildren, h₁, h₂, String.append_assoc]
end

/-- **The well-formedness theorem:** given no `unsafeRaw` use, `render` always produces
well-formed markup (`WellFormedHtml`): balanced tags, no unescaped `<`/`>` anywhere outside of
tag delimiters, and every attribute run of the shape its dialect admits. At `.xhtml` that output
is XML. -/
theorem Node.render_wellFormed {cat : Category} (n : Node cat) (h : n.hasVerbatim = false)
    (dialect : Dialect := .html5) (lineBreaks : Bool := false) :
    WellFormedHtml dialect (n.render dialect lineBreaks) := by
  obtain ⟨y, hy, hwf⟩ := renderInto_appends dialect lineBreaks n.erase h ""
  simpa [Node.render, hy] using hwf

end Html

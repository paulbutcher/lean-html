/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import HtmlModel
public import Html.Escape

@[expose] public section

namespace Html

open HtmlModel

/-- An HTML content-model category, as lean-html-model defines them. -/
abbrev Category := HtmlModel.Html.Cat

/-- A node valid wherever category `cat` is, carrying its attributes as each element's payload.
Its content model was checked by lean-html-model when it was built. -/
abbrev Node (cat : Category) := Web.Node Attrs (.html cat)

/-- A node with its typing erased, as rendering consumes it. -/
abbrev Tree := Web.Tree Attrs

/-- String literals stand for a text node wherever text is valid, so callers write `"hi"` among
a tag's children instead of `Node.text "hi"`. -/
instance {cat : Category} [h : Web.Widens .text (.html cat)] (s : String) : CoeT String s (Node cat) :=
  ⟨Web.widen (.text s) h.widens⟩

/-- A string of ASCII whitespace, the only text valid among children that aren't phrasing
content. For a literal, `{ val := "\n" }` discharges the proof by `decide`. -/
structure Whitespace where
  val : String
  isWhitespace : HtmlModel.isWhitespace val = true := by decide

namespace Whitespace

def newline : Whitespace := { val := "\n" }

def ofString? (s : String) : Option Whitespace :=
  if h : HtmlModel.isWhitespace s then some ⟨s, h⟩ else none

end Whitespace

instance {cat : Category} [h : Web.Widens .ws (.html cat)] (w : Whitespace) :
    CoeT Whitespace w (Node cat) :=
  ⟨Web.widen (Web.ws w.val w.isWhitespace) h.widens⟩

namespace Node

/-- A text node. Text is phrasing content, and reaches flow content by widening. -/
def text (s : String) : Node .phrasing := Web.widen (.text s)

/-- Inter-element whitespace, the only text valid among children that aren't phrasing
content. -/
def whitespace {cat : Category} [h : Web.Widens .ws (.html cat)] (w : Whitespace) : Node cat :=
  Web.widen (Web.ws w.val w.isWhitespace) h.widens

/-- Verbatim, unescaped markup, trusted as-is, usable as content of any category.
Misuse can lead to XSS issues. -/
def unsafeRaw {cat : Category} (s : String) : Node cat := .verbatim s

end Node

end Html

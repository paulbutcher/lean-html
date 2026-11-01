/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import HtmlRenderer.Node

@[expose] public section

namespace Html

open HtmlModel

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

/-- Inter-element whitespace, the only text valid among children that aren't phrasing
content. -/
def whitespace {cat : Category} [h : Web.Widens .ws (.html cat)] (w : Whitespace) : Node cat :=
  Web.widen (Web.ws w.val w.isWhitespace) h.widens

end Node

end Html

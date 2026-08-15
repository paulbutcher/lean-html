/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import Html.Escape

namespace HtmlTests

open Html

#guard escape "<script>" = "&lt;script&gt;"
#guard escape "\"onclick=\"" = "&quot;onclick=&quot;"
#guard escape "a & b" = "a &amp; b"
#guard escape "" = ""
#guard escape "héllo wörld 日本語" = "héllo wörld 日本語"
#guard escape "&<>\"" = "&amp;&lt;&gt;&quot;"
#guard escape "&amp;" = "&amp;amp;"  -- already-escaped input isn't special-cased; re-escaping & first is correct

private theorem foldl_append_eq (l : List String) :
    ∀ acc : String, l.foldl (· ++ ·) acc = acc ++ l.foldl (· ++ ·) "" := by
  induction l with
  | nil => simp
  | cons a as ih =>
    intro acc
    simp only [List.foldl_cons, String.empty_append]
    rw [ih a, ih (acc ++ a), String.append_assoc]

private theorem join_append (l1 l2 : List String) :
    String.join (l1 ++ l2) = String.join l1 ++ String.join l2 := by
  unfold String.join
  rw [List.foldl_append, foldl_append_eq l2]

/-- **Compositionality:** escaping two fragments and concatenating the results
is the same as escaping their concatenation directly. No double-escaping
and no under-escaping happens at the fragment boundary. -/
theorem escape_append (a b : String) : escape (a ++ b) = escape a ++ escape b := by
  unfold escape
  rw [String.toList_append, List.map_append, join_append]

/-- **The paired renderer-side invariant:** `renderAttr` always embeds the escaped
value between two literal `"` delimiters. Combined with `escape_safe`, this rules
out attribute-value breakout: the value can never contain a raw `"` to close the
attribute early, nor a raw `<`/`>` to open a new tag. -/
theorem renderAttr_safe (name value : String) :
    renderAttr name value = s!" {name}=\"" ++ escape value ++ "\"" ∧
      ∀ c ∈ (escape value).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨rfl, escape_safe value⟩

/-- **Compositionality of attribute rendering:** an `Attrs` list renders to the
concatenation of its parts' renderings. Every `<Tag>Attrs.render` in
`Html/Attrs.lean` is a `++` chain over the primitives below, so this plus the
primitive laws pins down each record's output without enumerating its fields. -/
theorem attrsRender_append (a b : Attrs) :
    Attrs.render (a ++ b) = Attrs.render a ++ Attrs.render b := by
  unfold Attrs.render
  rw [List.map_append, join_append]

theorem attrsRender_reqAttr (name value : String) :
    Attrs.render (reqAttr name value) = renderAttr (sanitizeAttrName name) value := rfl

theorem attrsRender_optAttr_none (name : String) :
    Attrs.render (optAttr name none) = "" := rfl

theorem attrsRender_optAttr_some (name value : String) :
    Attrs.render (optAttr name (some value)) = renderAttr (sanitizeAttrName name) value := rfl

/-- A `false` boolean attribute contributes nothing at all: never `name="false"`,
which the HTML spec reads as the attribute being *present*, hence true. -/
theorem attrsRender_flagAttr_false (name : String) :
    Attrs.render (flagAttr name false) = "" := rfl

theorem attrsRender_flagAttr_true (name : String) :
    Attrs.render (flagAttr name true) = " " ++ sanitizeAttrName name := rfl

end HtmlTests

/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html.Escape
meta import Html.Escape

namespace HtmlTests

open Html

#guard escape "<script>" = "&lt;script&gt;"
#guard escape "\"onclick=\"" = "&quot;onclick=&quot;"
#guard escape "a & b" = "a &amp; b"
#guard escape "" = ""
#guard escape "héllo wörld 日本語" = "héllo wörld 日本語"
#guard escape "&<>\"" = "&amp;&lt;&gt;&quot;"
#guard escape "&amp;" = "&amp;amp;"  -- already-escaped input isn't special-cased; re-escaping & first is correct

/-- **Compositionality:** escaping two fragments and concatenating the results
is the same as escaping their concatenation directly. No double-escaping
and no under-escaping happens at the fragment boundary. -/
theorem escape_append (a b : String) : escape (a ++ b) = escape a ++ escape b := by
  unfold escape
  rw [String.toList_append, List.map_append, String.join_append]

/-- **The paired renderer-side invariant:** `renderAttr` always embeds the escaped
value between two literal `"` delimiters. Combined with `escape_safe`, this rules
out attribute-value breakout: the value can never contain a raw `"` to close the
attribute early, nor a raw `<`/`>` to open a new tag. -/
theorem renderAttr_safe (name value : String) :
    renderAttr name value = s!" {name}=\"" ++ escape value ++ "\"" ∧
      ∀ c ∈ (escape value).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨rfl, escape_safe value⟩

private abbrev attrKey (f : AttrFragment) : String := attrNameKey (sanitizeAttrName f.name)

/-- **First wins:** a fragment whose name duplicates the one before it is
dropped, whatever either holds and whatever follows. -/
theorem attrsRender_firstWins (f g : AttrFragment) (rest : Attrs) (dialect : Dialect)
    (h : attrKey g = attrKey f) :
    Attrs.render (f :: g :: rest) dialect = Attrs.render (f :: rest) dialect := by
  simp [Attrs.render, Attrs.render.go, h]

/-- `go` consults `seen` only for the keys of the fragments it's given. -/
private theorem go_congr (dialect : Dialect) (l : Attrs) (s t : List String)
    (h : ∀ k ∈ l.map attrKey, k ∈ s ↔ k ∈ t) :
    Attrs.render.go dialect s l = Attrs.render.go dialect t l := by
  induction l generalizing s t with
  | nil => rfl
  | cons f rest ih =>
    have hf := h (attrKey f) List.mem_cons_self
    have hrest (s t : List String) (h' : ∀ k ∈ (f :: rest).map attrKey, k ∈ s ↔ k ∈ t) :=
      ih s t fun k hk => h' k (List.mem_cons_of_mem _ hk)
    simp only [Attrs.render.go]
    by_cases hs : attrKey f ∈ s
    · simp only [hs, hf.mp hs, ↓reduceIte]
      exact hrest s t h
    · simp only [hs, mt hf.mpr hs, ↓reduceIte]
      congr 1
      exact hrest _ _ fun k hk => by simp only [List.mem_cons, h k hk]

private theorem go_append (dialect : Dialect) (s : List String) (a b : Attrs) :
    Attrs.render.go dialect s (a ++ b)
      = Attrs.render.go dialect s a ++ Attrs.render.go dialect (a.map attrKey ++ s) b := by
  induction a generalizing s with
  | nil => simp [Attrs.render.go]
  | cons f rest ih =>
    simp only [List.cons_append, Attrs.render.go]
    by_cases hs : attrKey f ∈ s
    · simp only [hs, ↓reduceIte]; rw [ih s]
      congr 1
      exact go_congr dialect b _ _ fun k _ => by grind
    · simp only [hs, ↓reduceIte]; rw [ih, String.append_assoc]
      congr 2
      exact go_congr dialect b _ _ fun k _ => by grind

/-- **Compositionality of attribute rendering:** an `Attrs` list renders to the
concatenation of its parts' renderings, given no name in one duplicates a name
in the other. Every `<Tag>Attrs.render` in `Html/Attrs.lean` is a `++` chain
of distinct names over the primitives below, so this plus the primitive laws
pins down each record's output without enumerating its fields. -/
theorem attrsRender_append (a b : Attrs) (dialect : Dialect)
    (h : ∀ k ∈ b.map attrKey, k ∉ a.map attrKey) :
    Attrs.render (a ++ b) dialect = Attrs.render a dialect ++ Attrs.render b dialect := by
  simp only [Attrs.render]
  rw [go_append]
  exact congrArg _ (go_congr dialect b _ _ fun k hk => by simp [h k hk])

#guard Attrs.render [.value "a b" "1", .value "a_b" "2"] .xhtml = " a_b=\"1\""
#guard Attrs.render [.value "id" "1", .value "ID" "2"] .xhtml = " id=\"1\""

theorem attrsRender_reqAttr (name value : String) (dialect : Dialect) :
    Attrs.render (reqAttr name value) dialect = renderAttr (sanitizeAttrName name) value := by
  simp [Attrs.render, Attrs.render.go, reqAttr, AttrFragment.render]

theorem attrsRender_optAttr_none (name : String) (dialect : Dialect) :
    Attrs.render (optAttr name none) dialect = "" := by
  simp [Attrs.render, Attrs.render.go, optAttr]

theorem attrsRender_optAttr_some (name value : String) (dialect : Dialect) :
    Attrs.render (optAttr name (some value)) dialect = renderAttr (sanitizeAttrName name) value := by
  simp [Attrs.render, Attrs.render.go, optAttr, AttrFragment.render]

/-- A `false` boolean attribute contributes nothing at all: never `name="false"`,
which the HTML spec reads as the attribute being *present*, hence true. -/
theorem attrsRender_flagAttr_false (name : String) (dialect : Dialect) :
    Attrs.render (flagAttr name false) dialect = "" := by
  simp [Attrs.render, Attrs.render.go, flagAttr]

theorem attrsRender_flagAttr_true_html5 (name : String) :
    Attrs.render (flagAttr name true) .html5 = " " ++ sanitizeAttrName name := by
  simp [Attrs.render, Attrs.render.go, flagAttr, AttrFragment.render, toString]

/-- The same attribute in XML, where a bare name isn't well-formed: HTML5 reads
`name="name"` as present, so the two renderings agree on meaning. -/
theorem attrsRender_flagAttr_true_xhtml (name : String) :
    Attrs.render (flagAttr name true) .xhtml
      = renderAttr (sanitizeAttrName name) (sanitizeAttrName name) := by
  simp [Attrs.render, Attrs.render.go, flagAttr, AttrFragment.render]

end HtmlTests

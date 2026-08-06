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
#guard renderAttr "class" "a\"b" = " class=\"a&quot;b\""
#guard renderAttr "href" "x" = " href=\"x\""

private def isDangerous (c : Char) : Bool := c == '<' || c == '>' || c == '"'

private theorem escapeChar_clean (c : Char) :
    ∀ c' ∈ (escapeChar c).toList, isDangerous c' = false := by
  unfold escapeChar
  split
  case h_1 => decide
  case h_2 => decide
  case h_3 => decide
  case h_4 => decide
  case h_5 =>
    intro c' hc'
    simp only [String.toList_singleton, List.mem_singleton] at hc'
    subst hc'
    unfold isDangerous
    simp_all

private theorem join_toList (l : List String) :
    ∀ acc : String, (l.foldl (· ++ ·) acc).toList = acc.toList ++ (l.map String.toList).flatten := by
  induction l with
  | nil => simp
  | cons a as ih =>
    intro acc
    simp only [List.foldl_cons, List.map_cons, List.flatten_cons]
    rw [ih (acc ++ a)]
    simp [String.toList_append, List.append_assoc]

/-- **The XSS-relevant safety property:** `escape`'s output never contains a raw
(unescaped) `<`, `>`, or `"`. This is what makes double-quote-delimited,
escaped attribute values and escaped text content safe against markup
breakout — see `renderAttr_safe`, `render_text_safe`, and
`render_textElement_safe` for the paired renderer-side invariants this
depends on. -/
theorem escape_safe (s : String) : ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' := by
  unfold escape String.join
  intro c hc
  rw [join_toList] at hc
  simp only [String.toList_empty, List.nil_append, List.mem_flatten, List.mem_map] at hc
  obtain ⟨cs, ⟨s0, ⟨c0, _hc0mem, hs0eq⟩, hcs0eq⟩, hcmem⟩ := hc
  have h := escapeChar_clean c0 c (hs0eq ▸ hcs0eq ▸ hcmem)
  unfold isDangerous at h
  simp only [Bool.or_eq_false_iff, beq_eq_false_iff_ne, ne_eq] at h
  exact ⟨h.1.1, h.1.2, h.2⟩

/-- **The paired renderer-side invariant:** `renderAttr` always embeds the escaped
value between two literal `"` delimiters. Combined with `escape_safe`, this rules
out attribute-value breakout: the value can never contain a raw `"` to close the
attribute early, nor a raw `<`/`>` to open a new tag. -/
theorem renderAttr_safe (name value : String) :
    renderAttr name value = s!" {name}=\"" ++ escape value ++ "\"" ∧
      ∀ c ∈ (escape value).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨rfl, escape_safe value⟩

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

/-- Unfolds `sanitizeAttrName`'s `let` so `split` can case on the `if` directly. -/
private theorem sanitizeAttrName_eq (s : String) :
    sanitizeAttrName s =
      if (s.map sanitizeAttrNameChar).isEmpty then "_" else s.map sanitizeAttrNameChar :=
  rfl

/-- **The rawAttrs-name-relevant safety property:** `sanitizeAttrName`'s output
consists entirely of `isAttrNameChar` characters -- no whitespace, `=`, `<`,
`>`, or quote can survive sanitization. This is what closes the `renderRawAttrs`
gap: an untrusted attribute name can no longer break out of the tag it's
rendered into (compare the `renderAttr_safe`/`escape_safe` pair, which gives
the same guarantee for attribute *values*). -/
theorem sanitizeAttrName_safe (s : String) :
    ∀ c ∈ (sanitizeAttrName s).toList, isAttrNameChar c = true := by
  rw [sanitizeAttrName_eq]
  split
  · decide
  · rw [String.toList_map]
    intro c hc
    obtain ⟨c', _, heq⟩ := List.mem_map.mp hc
    simp only [sanitizeAttrNameChar] at heq
    split at heq <;> subst heq <;> first | assumption | decide

end HtmlTests

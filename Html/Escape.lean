/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

namespace Html

/-- Escapes `&`, `<`, `>`, `"` -/
def escapeChar (c : Char) : String :=
  match c with
  | '&' => "&amp;" -- Must be first
  | '<' => "&lt;"
  | '>' => "&gt;"
  | '"' => "&quot;"
  | c    => String.singleton c

/-- Escape a whole string. -/
def escape (s : String) : String :=
  String.join (s.toList.map escapeChar)

/-- Render one attribute as `name="escaped value"`, with a leading space
so callers can concatenate these directly after a tag name. -/
def renderAttr (name value : String) : String :=
  s!" {name}=\"{escape value}\""

/-- Characters allowed in a sanitized attribute name: ASCII letters,
digits, `-`, `_`, `:`, `.`, enough for real-world names (`data-x`,
`aria-label`, `v-on:click`) while excluding anything (whitespace, `=`,
`<`, `>`, quotes) that would let a name break out of the tag it's
rendered into. -/
def isAttrNameChar (c : Char) : Bool :=
  c.isAlphanum || c == '-' || c == '_' || c == ':' || c == '.'

def sanitizeAttrNameChar (c : Char) : Char :=
  if isAttrNameChar c then c else '_'

/-- Coerce an arbitrary string into a safe attribute name by replacing
every disallowed character with `_`, falling back to `_` itself when that
leaves nothing. Mirrors `escape`'s role for attribute *values*: callers
can pass untrusted strings as `rawAttrs` names (`Html/Attrs.lean`'s
`renderRawAttrs`) and still get well-formed output. -/
def sanitizeAttrName (s : String) : String :=
  let sanitized := s.map sanitizeAttrNameChar
  if sanitized.isEmpty then "_" else sanitized

/-- One rendered attribute: a `name="value"` pair, or a bare boolean flag
(`name`, no value). Names and values are stored *unsanitized/unescaped*;
`Attrs.render` is the only place that happens. -/
inductive AttrFragment where
  | value (name val : String)
  | flag (name : String)

/-- An attribute list, in emission order. This is the only type
`Html/Node.lean`'s element constructors accept for attributes; unlike a
bare `attrsStr : String`, an `Attrs` value can't smuggle in an unescaped
`<`/`>`/`"` no matter what strings its fragments carry, because turning it
into a string always goes through `Attrs.render` below. -/
abbrev Attrs := List AttrFragment

/-- Render an attribute list to a string, sanitizing every name and
escaping every value as it goes. This is the single choke point that makes
any `Attrs` value safe to splice directly after a tag name. -/
def Attrs.render (attrs : Attrs) : String :=
  String.join (attrs.map fun
    | .value name val => renderAttr (sanitizeAttrName name) val
    | .flag name => s!" {sanitizeAttrName name}")

/-- A single required `name="value"` attribute. -/
def reqAttr (name value : String) : Attrs := [.value name value]

/-- A single `name="value"` attribute when present, nothing otherwise. -/
def optAttr (name : String) : Option String → Attrs
  | none => []
  | some v => [.value name v]

/-- A single bare `name` flag when `true`, nothing otherwise. -/
def flagAttr (name : String) (b : Bool) : Attrs :=
  if b then [.flag name] else []

/-- Render arbitrary `(name, value)` pairs: values escaped, names sanitized
(see `sanitizeAttrName`) so a name containing e.g. a space or `"` can't
break out of the tag it's rendered into. -/
def renderRawAttrs (attrs : List (String × String)) : String :=
  Attrs.render (attrs.map fun (n, v) => .value n v)

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
breakout, given a renderer that keeps the escaped value between its two
literal `"` delimiters. -/
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

private theorem sanitizeAttrName_eq (s : String) :
    sanitizeAttrName s =
      if (s.map sanitizeAttrNameChar).isEmpty then "_" else s.map sanitizeAttrNameChar :=
  rfl

/-- **The rawAttrs-name-relevant safety property:** `sanitizeAttrName`'s output
consists entirely of `isAttrNameChar` characters; no whitespace, `=`, `<`,
`>`, or quote can survive sanitization. This is what closes the `renderRawAttrs`
gap: an untrusted attribute name can no longer break out of the tag it's
rendered into (compare `escape_safe`, which gives the same guarantee for
attribute *values*). -/
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

private theorem isAttrNameChar_safe (c : Char) (h : isAttrNameChar c = true) : c ≠ '<' ∧ c ≠ '>' := by
  unfold isAttrNameChar at h
  constructor <;> (rintro rfl; simp_all)

/-- Concatenation preserves "every character satisfies `P`". -/
private theorem append_safe {P : Char → Prop} {a b : String}
    (ha : ∀ c ∈ a.toList, P c) (hb : ∀ c ∈ b.toList, P c) : ∀ c ∈ (a ++ b).toList, P c := by
  intro c hc
  rw [String.toList_append, List.mem_append] at hc
  exact hc.elim (ha c) (hb c)

/-- `String.join` over a mapped list preserves "every character satisfies `P`",
given every individual mapped piece does. -/
private theorem join_map_safe {α : Type} {P : Char → Prop} (f : α → String) (l : List α)
    (h : ∀ x ∈ l, ∀ c ∈ (f x).toList, P c) : ∀ c ∈ (String.join (l.map f)).toList, P c := by
  unfold String.join
  intro c hc
  rw [join_toList] at hc
  simp only [String.toList_empty, List.nil_append, List.mem_flatten, List.mem_map] at hc
  obtain ⟨cs, ⟨y, ⟨x, hxmem, hxeq⟩, hycs⟩, hccs⟩ := hc
  exact h x hxmem c (hxeq ▸ hycs ▸ hccs)

private theorem AttrFragment.render_safe (f : AttrFragment) :
    ∀ c ∈ (match f with
      | .value name val => renderAttr (sanitizeAttrName name) val
      | .flag name => s!" {sanitizeAttrName name}").toList, c ≠ '<' ∧ c ≠ '>' := by
  cases f with
  | value name val =>
    have heq : renderAttr (sanitizeAttrName name) val
        = " " ++ sanitizeAttrName name ++ "=\"" ++ escape val ++ "\"" := rfl
    simp only [heq]
    apply append_safe (P := fun c => c ≠ '<' ∧ c ≠ '>')
    apply append_safe (P := fun c => c ≠ '<' ∧ c ≠ '>')
    apply append_safe (P := fun c => c ≠ '<' ∧ c ≠ '>')
    apply append_safe (P := fun c => c ≠ '<' ∧ c ≠ '>')
    · decide
    · exact fun c hc => isAttrNameChar_safe c (sanitizeAttrName_safe name c hc)
    · decide
    · exact fun c hc => (escape_safe val c hc).elim fun h1 h2 => ⟨h1, h2.1⟩
    · decide
  | flag name =>
    have heq : s!" {sanitizeAttrName name}" = " " ++ sanitizeAttrName name := rfl
    simp only [heq]
    apply append_safe (P := fun c => c ≠ '<' ∧ c ≠ '>')
    · decide
    · exact fun c hc => isAttrNameChar_safe c (sanitizeAttrName_safe name c hc)

/-- **The well-formedness-relevant safety property:** no matter what
fragments an `Attrs` value contains, `Attrs.render`'s output never contains
a raw `<` or `>`; every name goes through `sanitizeAttrName_safe`, every
value through `escape_safe`. This is what makes `Node.elementOf`'s
attribute string safe to splice directly after a tag name (see
`Node.WellFormed` in `Html/Node.lean`). -/
theorem Attrs.render_safe (attrs : Attrs) : ∀ c ∈ (Attrs.render attrs).toList, c ≠ '<' ∧ c ≠ '>' := by
  unfold Attrs.render
  exact join_map_safe _ attrs fun f _ => AttrFragment.render_safe f

end Html

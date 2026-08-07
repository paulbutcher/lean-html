import Html.Node
import HtmlTests.Escape

open HtmlTests

namespace Html

/-- The well-formed-HTML grammar that `render`/`renderPretty` are proved to
produce, given no `unsafeRaw` use (`Node.render_wellFormed`). A string is
well-formed if every `<` it contains either opens a void tag `<tag attrs>`
with nothing else following, or opens a tag immediately matched, once its
contents are exhausted, by a literal `</tag>` -- and nowhere else does a
`<` or `>` appear at all. This is a spec of the *output string*, stated
independently of `Node`/`Repr`. -/
inductive WellFormedHtml : String → Prop where
  | text {s : String} (h : ∀ c ∈ s.toList, c ≠ '<' ∧ c ≠ '>') : WellFormedHtml s
  | void {tag attrsStr : String} (h : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedHtml s!"<{tag}{attrsStr}>"
  | elem {tag attrsStr inner : String}
      (hattrs : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>') (hinner : WellFormedHtml inner) :
      WellFormedHtml (s!"<{tag}{attrsStr}>" ++ inner ++ s!"</{tag}>")
  | append {a b : String} (ha : WellFormedHtml a) (hb : WellFormedHtml b) :
      WellFormedHtml (a ++ b)

namespace Node

/-- Folding a "rebase-able" `g` (one where feeding it a starting
accumulator is the same as prepending that accumulator to what `g`
produces from `""`) over a list is itself rebase-able: running the fold
from `acc` is the same as prepending `acc` to running it from `""`. Used
to pull `renderCompactInto`'s threaded accumulator out from under a
`List.foldl` over a node's children. -/
private theorem foldl_rebase {α : Type} (g : α → String → String) :
    (l : List α) → (∀ x ∈ l, ∀ (s : String), g x s = s ++ g x "") →
      ∀ acc : String, l.foldl (fun acc x => g x acc) acc = acc ++ l.foldl (fun acc x => g x acc) ""
  | [], _, _ => by simp
  | x :: xs, hg, acc => by
    have hgx : ∀ s, g x s = s ++ g x "" := hg x (List.mem_cons_self ..)
    have hgxs : ∀ y ∈ xs, ∀ s, g y s = s ++ g y "" := fun y hy => hg y (List.mem_cons_of_mem _ hy)
    simp only [List.foldl_cons]
    rw [hgx acc, foldl_rebase g xs hgxs (acc ++ g x ""),
        foldl_rebase g xs hgxs (g x ""), String.append_assoc]

/-- `renderCompactInto`'s threaded accumulator is just a prefix: rendering
into an existing accumulator is the same as prepending that accumulator to
what rendering from scratch produces. -/
private theorem renderCompactInto_append (r : Repr) (acc : String) :
    renderCompactInto r acc = acc ++ renderCompactInto r "" :=
  match r with
  | .leaf _ => by simp [renderCompactInto]
  | .void _ _ => by simp [renderCompactInto]
  | .rawText _ _ _ => by simp [renderCompactInto, String.append_assoc]
  | .elem tag attrsStr children _ => by
    simp only [renderCompactInto, String.empty_append]
    have hg : ∀ c ∈ children, ∀ s, renderCompactInto c s = s ++ renderCompactInto c "" :=
      fun c _ s => renderCompactInto_append c s
    rw [foldl_rebase renderCompactInto children hg (acc ++ s!"<{tag}{attrsStr}>"),
        foldl_rebase renderCompactInto children hg (s!"<{tag}{attrsStr}>")]
    simp [String.append_assoc]

/-- A `Repr` built without `unsafeRaw`: every leaf/text/attribute string it
carries avoids raw `<`/`>`. This can't be checked after the fact for an
arbitrary `Node` (a `text "hi"` and an `unsafeRaw "hi"` are literally the
same value once built), so `WellFormed` below isn't a runtime check -- it's
established compositionally, per constructor, by the lemmas that follow
(`text_wellFormed`, `elementOf_wellFormed`, ...), each of which mirrors a
step of building a `Node` without ever calling `unsafeRaw`. There's
deliberately no `unsafeRaw_wellFormed`. -/
private inductive WellFormedRepr : Repr → Prop where
  | leaf {s : String} (h : ∀ c ∈ s.toList, c ≠ '<' ∧ c ≠ '>') : WellFormedRepr (.leaf s)
  | void {tag attrsStr : String} (h : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedRepr (.void tag attrsStr)
  | rawText {tag attrsStr content : String}
      (hattrs : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>')
      (hcontent : ∀ c ∈ content.toList, c ≠ '<' ∧ c ≠ '>') :
      WellFormedRepr (.rawText tag attrsStr content)
  | elem {tag attrsStr : String} {children : List Repr} {block : Bool}
      (hattrs : ∀ c ∈ attrsStr.toList, c ≠ '<' ∧ c ≠ '>')
      (hchildren : ∀ c ∈ children, WellFormedRepr c) :
      WellFormedRepr (.elem tag attrsStr children block)

/-- A `Node` built without `unsafeRaw` -- see `WellFormedRepr`. -/
def WellFormed (n : Node cat) : Prop := WellFormedRepr n.repr

private theorem foldl_wellFormed (children : List Repr)
    (hind : ∀ c ∈ children, ∀ acc, WellFormedHtml acc → WellFormedHtml (renderCompactInto c acc)) :
    ∀ acc, WellFormedHtml acc →
      WellFormedHtml (children.foldl (fun acc c => renderCompactInto c acc) acc) := by
  induction children with
  | nil => intro acc hacc; simpa using hacc
  | cons c cs ih =>
    intro acc hacc
    simp only [List.foldl_cons]
    exact ih (fun c' hc' => hind c' (List.mem_cons_of_mem _ hc'))
      (renderCompactInto c acc) (hind c (List.mem_cons_self ..) acc hacc)

private theorem renderCompactInto_wellFormed (r : Repr) (hr : WellFormedRepr r) :
    ∀ acc, WellFormedHtml acc → WellFormedHtml (renderCompactInto r acc) := by
  induction hr with
  | leaf h => intro acc hacc; simp only [renderCompactInto]; exact hacc.append (.text h)
  | void h => intro acc hacc; simp only [renderCompactInto]; exact hacc.append (.void h)
  | rawText hattrs hcontent =>
    intro acc hacc
    simp only [renderCompactInto]
    simpa [toString, String.append_assoc] using hacc.append (.elem hattrs (.text hcontent))
  | elem hattrs hchildren ih =>
    rename_i tag attrsStr children block
    intro acc hacc
    simp only [renderCompactInto]
    have hg : ∀ c ∈ children, ∀ s, renderCompactInto c s = s ++ renderCompactInto c "" :=
      fun c _ s => renderCompactInto_append c s
    rw [foldl_rebase renderCompactInto children hg (acc ++ s!"<{tag}{attrsStr}>")]
    have hinner : WellFormedHtml (children.foldl (fun acc c => renderCompactInto c acc) "") :=
      foldl_wellFormed children ih "" (.text (by simp))
    simpa [String.append_assoc] using hacc.append (.elem hattrs hinner)

/-- **The well-formedness theorem:** given no `unsafeRaw` use, `render`
always produces well-formed HTML (`WellFormedHtml`) -- balanced tags, with
no unescaped `<`/`>` anywhere outside of tag delimiters. -/
theorem render_wellFormed (n : Node cat) (h : WellFormed n) : WellFormedHtml n.render :=
  renderCompactInto_wellFormed n.repr h "" (.text (by simp))

/-- Drops the `≠ '"'` conjunct `escape_safe` gives, down to what
`WellFormed`/`WellFormedHtml` need. -/
private theorem escape_safe' (s : String) : ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' :=
  fun c hc => ⟨(escape_safe s c hc).1, (escape_safe s c hc).2.1⟩

theorem text_wellFormed (s : String) : WellFormed (text s : Node cat) :=
  .leaf (escape_safe' s)

theorem textElement_wellFormed (cat : Category) (tag content : String) (attrs : Attrs) :
    WellFormed (textElement cat tag content attrs) :=
  .rawText (Attrs.render_safe attrs) (escape_safe' content)

theorem voidElement_wellFormed (cat : Category) (tag : String) (attrs : Attrs) :
    WellFormed (voidElement cat tag attrs) :=
  .void (Attrs.render_safe attrs)

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
  .elem (Attrs.render_safe attrs) fun r hr => by
    obtain ⟨c, hc, hceq⟩ := List.mem_map.mp hr
    exact hceq ▸ h c hc

theorem element_wellFormed (cat : Category) (tag : String) (children : List (Node cat))
    (attrs : Attrs) (h : ∀ c ∈ children, WellFormed c) : WellFormed (element cat tag children attrs) :=
  elementOf_wellFormed cat cat tag children attrs h

end Node
end Html

namespace HtmlTests

open Html

/-- **Paired renderer-side invariant for `text`:** a `text` leaf renders to exactly
its escaped content, with nothing else spliced in. Combined with `escape_safe`,
a `text` leaf can never inject a raw `<`/`>` into its surrounding markup. -/
theorem render_text_safe (cat : Category) (s : String) :
    Node.render (Node.text (cat := cat) s) = escape s ∧
      ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨Node.render_text s, escape_safe s⟩

/-- **Paired renderer-side invariant for `textElement`:** the escaped content is
always embedded between the literal opening and closing tags. Combined with
`escape_safe`, the content can never contain a raw `<`/`>`, so it can't
prematurely close `</tag>` or open nested markup (the RCDATA safety property). -/
theorem render_textElement_safe (cat : Category) (tag content : String) (attrs : Attrs) :
    Node.render (Node.textElement cat tag content attrs)
      = s!"<{tag}{Attrs.render attrs}>" ++ escape content ++ s!"</{tag}>" ∧
      ∀ c ∈ (escape content).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨Node.render_textElement cat tag content attrs, escape_safe content⟩

-- **Demonstration of `render_wellFormed`:** a concrete tree, built purely from
-- the typed constructors (no `unsafeRaw`), is `WellFormed` by chaining
-- `element_wellFormed`/`text_wellFormed` over its shape -- and `render_wellFormed`
-- then turns that into a `WellFormedHtml` guarantee for the rendered string.
-- Real tag-function trees (`div [...]`, `p [...]`, ...) compose the same way,
-- since every tag function bottoms out in exactly these same primitives.
example : Node.WellFormed
    (Node.element .flow "div" [Node.text "hi", Node.element .flow "p" ([] : List (Node .flow))]) := by
  apply Node.element_wellFormed
  intro c hc
  simp only [List.mem_cons, List.not_mem_nil, or_false] at hc
  rcases hc with hc | hc <;> subst hc
  · exact Node.text_wellFormed "hi"
  · exact Node.element_wellFormed .flow "p" [] [] (by simp)

example : WellFormedHtml (Node.render
    (Node.element .flow "div" [Node.text "hi", Node.element .flow "p" ([] : List (Node .flow))])) :=
  Node.render_wellFormed _ (by
    apply Node.element_wellFormed
    intro c hc
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hc
    rcases hc with hc | hc <;> subst hc
    · exact Node.text_wellFormed "hi"
    · exact Node.element_wellFormed .flow "p" [] [] (by simp))

#guard Node.render (Node.element .flow "div" []) = "<div></div>"
#guard Node.render (Node.element .flow "div" [Node.element .flow "p" []]) = "<div><p></p></div>"
#guard Node.render (Node.voidElement .flow "br") = "<br>"
#guard Node.render
    (Node.element .flow "ul" [Node.element .flow "li" [], Node.element .flow "li" []])
  = "<ul><li></li><li></li></ul>"
#guard Node.render ((Node.element .phrasing "span" [] : Node .phrasing) : Node .flow) = "<span></span>"

-- String literals coerce directly to a `text` leaf (no `Node.text` needed).
#guard Node.render (Node.element .flow "p" [("hi" : Node .flow)]) = "<p>hi</p>"

-- Pretty-printing: empty and void elements stay one line.
#guard Node.renderPretty (Node.element .flow "div" []) = "<div></div>"
#guard Node.renderPretty (Node.voidElement .flow "br") = "<br>"

-- A lone leaf child (text or `unsafeRaw`) isn't worth exploding onto its
-- own line, whether the element's declared content category is flow or
-- phrasing.
#guard Node.renderPretty (Node.element .flow "li" [Node.text "one"]) = "<li>one</li>"
#guard Node.renderPretty (Node.element .flow "div" [(Node.unsafeRaw "<b>x</b>" : Node .flow)])
  = "<div><b>x</b></div>"

-- A lone *structured* (non-leaf) child, or a void child, does get its own
-- indented line
#guard Node.renderPretty (Node.element .flow "div" [Node.element .flow "p" []])
  = "<div>\n  <p></p>\n</div>"
#guard Node.renderPretty (Node.element .flow "div" [(Node.voidElement .flow "hr" : Node .flow)])
  = "<div>\n  <hr>\n</div>"

-- Multiple flow children: one per line, indented one level deeper than the
-- parent, closing tag back at the parent's own indentation.
#guard Node.renderPretty
    (Node.element .flow "div" [Node.element .flow "p" [], Node.element .flow "p" []])
  = "<div>\n  <p></p>\n  <p></p>\n</div>"

-- Nesting increases indentation by one `unit` per level.
#guard Node.renderPretty
    (Node.element .flow "div"
      [Node.element .flow "div" [Node.element .flow "p" [], Node.element .flow "p" []]])
  = "<div>\n  <div>\n    <p></p>\n    <p></p>\n  </div>\n</div>"

-- Phrasing children are laid out inline, exactly like compact rendering,
-- regardless of how many there are or how deep the surrounding tree is --
-- whitespace between text/inline runs is visible in rendered output, so
-- the pretty-printer must never inject any.
#guard Node.renderPretty
    (Node.elementOf .flow .phrasing "p"
      [Node.text "Hello, ", (Node.element .phrasing "strong" [Node.text "world"] : Node .phrasing)])
  = "<p>Hello, <strong>world</strong></p>"
#guard Node.renderPretty
    (Node.element .flow "div"
      [Node.elementOf .flow .phrasing "p"
        [Node.text "Hello, ", (Node.element .phrasing "strong" [Node.text "world"] : Node .phrasing)]])
  = "<div>\n  <p>Hello, <strong>world</strong></p>\n</div>"

-- `unit` is configurable (default two spaces).
#guard Node.renderPretty (Node.element .flow "div" [Node.element .flow "p" []]) "    "
  = "<div>\n    <p></p>\n</div>"

end HtmlTests

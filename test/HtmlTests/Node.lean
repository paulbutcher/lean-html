/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/

import Html.Node

namespace HtmlTests

open Html

-- **Demonstration of `render_wellFormed`:** a concrete tree, built purely from
-- the typed constructors (no `unsafeRaw`), is `WellFormed` by chaining
-- `element_wellFormed`/`text_wellFormed` over its shape; `render_wellFormed`
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

example : WellFormedHtml false (Node.render
    (Node.element .flow "div" [Node.text "hi", Node.element .flow "p" ([] : List (Node .flow))])) :=
  Node.render_wellFormed _ (by
    apply Node.element_wellFormed
    intro c hc
    simp only [List.mem_cons, List.not_mem_nil, or_false] at hc
    rcases hc with hc | hc <;> subst hc
    · exact Node.text_wellFormed "hi"
    · exact Node.element_wellFormed .flow "p" [] [] (by simp))

theorem render_text (s : String) : (Node.text s : Node cat).render = escape s := by
  simp [Node.render, Node.text, Node.renderCompactInto]

theorem render_textElement (cat : Category) (tag content : String) (attrs : Attrs) :
    (Node.textElement cat tag content attrs).render
      = s!"<{tag}{Attrs.render attrs}>" ++ escape content ++ s!"</{tag}>" := by
  simp [Node.render, Node.textElement, Node.renderCompactInto, toString, String.append_assoc]

/-- **Paired renderer-side invariant for `text`:** a `text` leaf renders to exactly
its escaped content, with nothing else spliced in. Combined with `escape_safe`,
a `text` leaf can never inject a raw `<`/`>` into its surrounding markup. -/
theorem render_text_safe (cat : Category) (s : String) :
    Node.render (Node.text (cat := cat) s) = escape s ∧
      ∀ c ∈ (escape s).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨render_text s, escape_safe s⟩

/-- **Paired renderer-side invariant for `textElement`:** the escaped content is
always embedded between the literal opening and closing tags. Combined with
`escape_safe`, the content can never contain a raw `<`/`>`, so it can't
prematurely close `</tag>` or open nested markup (the RCDATA safety property). -/
theorem render_textElement_safe (cat : Category) (tag content : String) (attrs : Attrs) :
    Node.render (Node.textElement cat tag content attrs)
      = s!"<{tag}{Attrs.render attrs}>" ++ escape content ++ s!"</{tag}>" ∧
      ∀ c ∈ (escape content).toList, c ≠ '<' ∧ c ≠ '>' ∧ c ≠ '"' :=
  ⟨render_textElement cat tag content attrs, escape_safe content⟩

#guard Node.render (Node.element .flow "div" []) = "<div></div>"
#guard Node.render (Node.element .flow "div" [Node.element .flow "p" []]) = "<div><p></p></div>"
#guard Node.render (Node.voidElement .flow "br") = "<br>"
#guard Node.render
    (Node.element .flow "ul" [Node.element .flow "li" [], Node.element .flow "li" []])
  = "<ul><li></li><li></li></ul>"
#guard Node.render ((Node.element .phrasing "span" [] : Node .phrasing) : Node .flow) = "<span></span>"

-- String literals coerce directly to a `text` leaf (no `Node.text` needed).
#guard Node.render (Node.element .flow "p" [("hi" : Node .flow)]) = "<p>hi</p>"

-- `selfClosingVoid` opts into XHTML-style void tags, and leaves everything
-- else (including non-void elements) untouched.
#guard Node.render (Node.voidElement .flow "br") (selfClosingVoid := true) = "<br />"
#guard Node.render
    (Node.element .flow "div" [(Node.voidElement .flow "hr" : Node .flow)]) (selfClosingVoid := true)
  = "<div><hr /></div>"
#guard Node.renderPretty (Node.voidElement .flow "br") (selfClosingVoid := true) = "<br />"

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

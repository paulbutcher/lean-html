/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html.Document
import Html.Node
import Html.Tags
meta import Html.Document
meta import Html.Node
meta import Html.Tags

namespace HtmlTests

open Html

-- Each structured tag function emits its parts in the order the content model fixes.
#guard Node.render (table [tr [td []]]) = "<table><tr><td></td></tr></table>"
#guard Node.render (details (summary ["More"]) [p ["Body"]])
  = "<details><summary>More</summary><p>Body</p></details>"
#guard Node.render (fieldset [p ["Body"]] (legend := legend ["Name"]))
  = "<fieldset><legend>Name</legend><p>Body</p></fieldset>"
#guard Node.render (figure [p ["Body"]] (caption := .first (figcaption ["Cap"])))
  = "<figure><figcaption>Cap</figcaption><p>Body</p></figure>"
#guard Node.render (figure [p ["Body"]] (caption := .last (figcaption ["Cap"])))
  = "<figure><p>Body</p><figcaption>Cap</figcaption></figure>"
#guard Node.render (dl (.groups [{ term := dt ["A"], moreTerms := [dt ["B"]], desc := dd ["a"] }]))
  = "<dl><dt>A</dt><dt>B</dt><dd>a</dd></dl>"
#guard Node.render (dl (.divs [dlDiv { term := dt ["A"], desc := dd ["a"], moreDescs := [dd ["b"]] }]))
  = "<dl><div><dt>A</dt><dd>a</dd><dd>b</dd></div></dl>"
#guard Node.render (picture [source { srcset := "a.webp" }] (img { src := "a.png", alt := "A" }))
  = "<picture><source srcset=\"a.webp\"><img src=\"a.png\" alt=\"A\"></picture>"
#guard Node.render (video (cat := .flow) [source { src := "a.mp4" }] [track { src := "a.vtt" }] ["No video"])
  = "<video><source src=\"a.mp4\"><track src=\"a.vtt\">No video</video>"
#guard Node.render (audio (cat := .phrasing) (.src "a.mp3") [track { src := "a.vtt" }])
  = "<audio src=\"a.mp3\"><track src=\"a.vtt\"></audio>"
#guard Node.render (ruby ⟨["漢"], .parenthesized (rp ["("]) (rt ["kan"], rp [")"])⟩ [⟨["字"], rt ["ji"]⟩])
  = "<ruby>漢<rp>(</rp><rt>kan</rt><rp>)</rp>字<rt>ji</rt></ruby>"
#guard Node.render (hgroup [p ["Over"]] (h1 ["Title"]) [p ["Under"]])
  = "<hgroup><p>Over</p><h1>Title</h1><p>Under</p></hgroup>"

-- A tree built dynamically needs no proof.
#guard
  let entries := [("a", "1"), ("b", "2")]
  Node.render (dl (.groups (entries.map fun (k, v) => { term := dt [k], desc := dd [v] })))
    = "<dl><dt>a</dt><dd>1</dd><dt>b</dt><dd>2</dd></dl>"
#guard
  let rows := [["1", "2"], ["3", "4"]]
  Node.render (table (rows.map fun r => tr (r.map fun (c : String) => td [c])))
    = "<table><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></table>"

-- Ruby annotations lay out inline, since whitespace beside them would render.
#guard let n := ruby ⟨["漢"], rt ["kan"]⟩
  n.renderPretty = n.render

-- Inter-element whitespace may stand among children that aren't phrasing content.
#guard Node.render (ul [li ["a"], Whitespace.newline, li ["b"]]) = "<ul><li>a</li>\n<li>b</li></ul>"
#guard Node.render (tr [td [], ({ val := " \n" } : Whitespace), td []]) = "<tr><td></td> \n<td></td></tr>"
#guard (Whitespace.ofString? " \t\n").isSome
#guard (Whitespace.ofString? " x ").isNone

-- `lineBreaks` gives cmark's layout, table sections and nested lists included.
#guard Node.render (table [tbody [tr [td ["1"]]]] (thead := thead [tr [th ["H"]]])) (lineBreaks := true)
  = "<table>\n<thead>\n<tr>\n<th>H</th>\n</tr>\n</thead>\n<tbody>\n<tr>\n<td>1</td>\n</tr>\n</tbody>\n</table>"
#guard Node.render (ul [li ["a", ul [li ["b"]]], li [p ["c"]]]) (lineBreaks := true)
  = "<ul>\n<li>a\n<ul>\n<li>b</li>\n</ul>\n</li>\n<li>\n<p>c</p>\n</li>\n</ul>"
-- It never separates phrasing siblings, nor puts a second break where text ends a line.
#guard Node.render (div ["a ", em ["b"], "\n", p ["c"]]) (lineBreaks := true)
  = "<div>a <em>b</em>\n<p>c</p>\n</div>"
#guard Node.render (p ["a ", em ["b"]]) (lineBreaks := true) = "<p>a <em>b</em></p>"

/-- `render_wellFormed` covers `lineBreaks`, and the structured tag functions' lemmas compose
like the primitives'. -/
example : WellFormedHtml .html5
    (Node.render (details (summary ["More"]) [p ["Body"]]) (lineBreaks := true)) :=
  Node.render_wellFormed _
    (Node.details_wellFormed _ _ _ _
      (Node.elementOf_wellFormed _ _ _ _ _ (by simpa using Node.text_wellFormed "More"))
      (by
        simp only [List.mem_singleton, forall_eq]
        exact Node.widen_wellFormed
          (Node.elementOf_wellFormed _ _ _ _ _ (by simpa using Node.text_wellFormed "Body"))))
    (lineBreaks := true)

-- Order: `<thead>` can't follow `<tbody>`, nor a caption stand among sections.
/--
error: Application type mismatch: The argument
  thead []
has type
  Node Category.tableHead
but is expected to have type
  Node Category.tableBody
in the application
  List.cons (thead [])
-/
#guard_msgs in
example : Node .flow := table [tbody [], thead []]

/--
error: Application type mismatch: The argument
  [caption [(Node.text "x").widen]]
has type
  List (Node Category.tableCaption)
but is expected to have type
  optParam TableBody (TableBody.sections [])
in the application
  @table [caption [(Node.text "x").widen]]
-/
#guard_msgs in
example : Node .flow := table [caption ["x"]]

-- Categories: each of these is valid only inside its own parent.
/--
error: Application type mismatch: The argument
  dt [(Node.text "x").widen]
has type
  Node Category.term
but is expected to have type
  Node Category.flow
in the application
  List.cons (dt [(Node.text "x").widen])
-/
#guard_msgs in
example : Node .flow := div [dt ["x"]]

/--
error: Application type mismatch: The argument
  dd [(Node.text "x").widen]
has type
  Node Category.description
but is expected to have type
  Node Category.flow
in the application
  List.cons (dd [(Node.text "x").widen])
-/
#guard_msgs in
example : Node .flow := div [dd ["x"]]

/--
error: Application type mismatch: The argument
  caption [(Node.text "x").widen]
has type
  Node Category.tableCaption
but is expected to have type
  Node Category.flow
in the application
  List.cons (caption [(Node.text "x").widen])
-/
#guard_msgs in
example : Node .flow := div [caption ["x"]]

/--
error: Application type mismatch: The argument
  summary [Node.text "x"]
has type
  Node Category.summary
but is expected to have type
  Node Category.flow
in the application
  List.cons (summary [Node.text "x"])
-/
#guard_msgs in
example : Node .flow := div [summary ["x"]]

/--
error: Application type mismatch: The argument
  legend [Node.text "x"]
has type
  Node Category.legend
but is expected to have type
  Node Category.flow
in the application
  List.cons (legend [Node.text "x"])
-/
#guard_msgs in
example : Node .flow := div [legend ["x"]]

/--
error: Application type mismatch: The argument
  figcaption [(Node.text "x").widen]
has type
  Node Category.figureCaption
but is expected to have type
  Node Category.flow
in the application
  List.cons (figcaption [(Node.text "x").widen])
-/
#guard_msgs in
example : Node .flow := div [figcaption ["x"]]

/--
error: Application type mismatch: The argument
  title "x"
has type
  Node Category.title
but is expected to have type
  Node Category.flow
in the application
  List.cons (title "x")
-/
#guard_msgs in
example : Node .flow := div [title "x"]

/--
error: Application type mismatch: The argument
  source
has type
  Node Category.source
but is expected to have type
  Node Category.flow
in the application
  List.cons source
-/
#guard_msgs in
example : Node .flow := div [source]

/--
error: Application type mismatch: The argument
  rt [Node.text "x"]
has type
  Node Category.rubyText
but is expected to have type
  Node Category.phrasing
in the application
  List.cons (rt [Node.text "x"])
-/
#guard_msgs in
example : Node .phrasing := span [rt ["x"]]

-- `<html>`: `<head>` then `<body>`.
/--
error: Application type mismatch: The argument
  body []
has type
  Node Category.body
but is expected to have type
  Node Category.head
in the application
  @document (body [])
-/
#guard_msgs in
example : String := document (body []) (head (title "T"))

-- `<details>`: a `<summary>` first, and only there.
/--
error: Application type mismatch: The argument
  p [Node.text "x"]
has type
  Node Category.paragraph
but is expected to have type
  Node Category.summary
in the application
  @details (p [Node.text "x"])
-/
#guard_msgs in
example : Node .flow := details (p ["x"])

/--
error: Application type mismatch: The argument
  summary [Node.text "b"]
has type
  Node Category.summary
but is expected to have type
  Node Category.flow
in the application
  List.cons (summary [Node.text "b"])
-/
#guard_msgs in
example : Node .flow := details (summary ["a"]) [summary ["b"]]

-- `<fieldset>`: a `<legend>` only as the optional first child.
/--
error: Application type mismatch: The argument
  legend [Node.text "x"]
has type
  Node Category.legend
but is expected to have type
  Node Category.flow
in the application
  List.cons (legend [Node.text "x"])
-/
#guard_msgs in
example : Node .flow := fieldset [legend ["x"]]

-- `<figure>`: a `<figcaption>` only first or last.
/--
error: Application type mismatch: The argument
  figcaption [(Node.text "x").widen]
has type
  Node Category.figureCaption
but is expected to have type
  Node Category.flow
in the application
  List.cons (figcaption [(Node.text "x").widen])
-/
#guard_msgs in
example : Node .flow := figure [p ["a"], figcaption ["x"], p ["b"]]

-- `<dl>`: terms before descriptions, and bare groups never mixed with `<div>`s.
/--
error: Type mismatch
  dd [(Node.text "x").widen]
has type
  Node Category.description
but is expected to have type
  Node Category.term
---
error: Type mismatch
  dt [(Node.text "y").widen]
has type
  Node Category.term
but is expected to have type
  Node Category.description
-/
#guard_msgs in
example : DlGroup := { term := dd ["x"], desc := dt ["y"] }

/--
error: Application type mismatch: The argument
  dt [(Node.text "x").widen]
has type
  Node Category.term
but is expected to have type
  Node Category.descriptionGroup
in the application
  List.cons (dt [(Node.text "x").widen])
-/
#guard_msgs in
example : Node .flow := dl (.divs [dt ["x"]])

-- `<picture>`: sources, then exactly one `<img>`.
/--
error: Application type mismatch: The argument
  img { src := "a", alt := "a" }
has type
  Node Category.image
but is expected to have type
  Node Category.source
in the application
  List.cons (img { src := "a", alt := "a" })
-/
#guard_msgs in
example : Node .phrasing := picture [img { src := "a", alt := "a" }] (source)

/--
error: Application type mismatch: The argument
  span []
has type
  Node Category.phrasing
but is expected to have type
  Node Category.image
in the application
  @picture [] (span [])
-/
#guard_msgs in
example : Node .phrasing := picture [] (span [])

-- Media: a `src` or `<source>`s, then `<track>`s, then fallback content.
/--
error: Application type mismatch: The argument
  track { src := "a.vtt" }
has type
  Node Category.track
but is expected to have type
  Node Category.source
in the application
  List.cons (track { src := "a.vtt" })
-/
#guard_msgs in
example : Node .flow := video (.sources [track { src := "a.vtt" }])

/--
error: Application type mismatch: The argument
  source
has type
  Node Category.source
but is expected to have type
  Node Category.flow
in the application
  List.cons source
-/
#guard_msgs in
example : Node .flow := video (.src "a.mp4") [] [source]

-- `<ruby>`: annotations aren't base text, and `<rp>` only brackets `<rt>`.
/--
error: Application type mismatch: The argument
  rt [Node.text "x"]
has type
  Node Category.rubyText
but is expected to have type
  Node Category.phrasing
in the application
  List.cons (rt [Node.text "x"])
-/
#guard_msgs in
example : Node .phrasing := ruby ⟨[rt ["x"]], rt ["y"]⟩

/--
error: Application type mismatch: The argument
  rt [Node.text "y"]
has type
  Node Category.rubyText
but is expected to have type
  Node Category.rubyParenthesis
in the application
  @RubyText.parenthesized (rt [Node.text "y"])
-/
#guard_msgs in
example : Node .phrasing := ruby ⟨["x"], .parenthesized (rt ["y"]) (rt ["y"], rp [")"])⟩

-- `<hgroup>`: exactly one heading.
/--
error: Application type mismatch: The argument
  h2 [Node.text "b"]
has type
  Node Category.heading
but is expected to have type
  Node Category.paragraph
in the application
  List.cons (h2 [Node.text "b"])
-/
#guard_msgs in
example : Node .flow := hgroup [] (h1 ["a"]) [h2 ["b"]]

/--
error: Application type mismatch: The argument
  p [Node.text "a"]
has type
  Node Category.paragraph
but is expected to have type
  Node Category.heading
in the application
  @hgroup [] (p [Node.text "a"])
-/
#guard_msgs in
example : Node .flow := hgroup [] (p ["a"])

-- Text: none outside phrasing content but whitespace, and whitespace never in place of a
-- required element.
/--
error: Application type mismatch: The argument
  "hello"
has type
  String
but is expected to have type
  Node Category.listItem
in the application
  List.cons "hello"
-/
#guard_msgs in
example : Node .flow := ul ["hello"]

/--
error: Application type mismatch: The argument
  "hello"
has type
  String
but is expected to have type
  Node Category.tableCell
in the application
  List.cons "hello"
-/
#guard_msgs in
example : Node .tableRow := tr ["hello"]

/--
error: could not synthesize default value for field 'isWhitespace' of 'Html.Whitespace' using tactics
---
error: Tactic `decide` proved that the proposition
  "x".toList.all isAsciiWhitespace = true
is false
-/
#guard_msgs in
example : Whitespace := { val := "x" }

/--
error: failed to synthesize instance of type class
  InterElement Category.summary

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
example : Node .flow := details (Node.whitespace .newline)

end HtmlTests

/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html
meta import Html

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

-- Heading content, `hgroup` included, may stand in `<summary>` and `<legend>`.
#guard Node.render (details (summary [h2 ["Title"], " more"]))
  = "<details><summary><h2>Title</h2> more</summary></details>"
#guard Node.render (fieldset [] (legend := legend [hgroup [] (h1 ["Title"])]))
  = "<fieldset><legend><hgroup><h1>Title</h1></hgroup></legend></fieldset>"

-- Script-supporting elements intermix through `List` arguments, and `<template>` among `<col>`s.
#guard Node.render (ul [li ["a"], script { src := "a.js" }]) = "<ul><li>a</li><script src=\"a.js\"></script></ul>"
#guard Node.render (table (.rows [tr [td ["1"], script { src := "a.js" }], template []])
    (colgroups := [colgroup (.cols [col, template []])]))
  = "<table><colgroup><col><template></template></colgroup>" ++
    "<tr><td>1</td><script src=\"a.js\"></script></tr><template></template></table>"
#guard Node.render (select [option "a", script { src := "a.js" }])
  = "<select><option>a</option><script src=\"a.js\"></script></select>"
#guard Node.render (picture [script { src := "a.js" }] (img { src := "a.png", alt := "A" }))
  = "<picture><script src=\"a.js\"></script><img src=\"a.png\" alt=\"A\"></picture>"
#guard Node.render (dl (.divs [dlDiv { term := dt ["A"], desc := dd ["a"] }, script { src := "a.js" }]))
  = "<dl><div><dt>A</dt><dd>a</dd></div><script src=\"a.js\"></script></dl>"

-- A `<colgroup>` has a `span` or `<col>` children.
#guard Node.render (table (colgroups := [colgroup (.span "2"), colgroup [col]]))
  = "<table><colgroup span=\"2\"></colgroup><colgroup><col></colgroup></table>"

-- In `<head>`, a `<noscript>` holds `link` and `meta`.
#guard document (head (title "T") [noscript [link { rel := "stylesheet", href := "n.css" }]])
    (body [noscript ["Enable scripts"]])
  = "<!DOCTYPE html><html><head><noscript><link rel=\"stylesheet\" href=\"n.css\"></noscript>" ++
    "<title>T</title></head><body><noscript>Enable scripts</noscript></body></html>"

-- A tree built dynamically needs no proof.
#guard
  let entries := [("a", "1"), ("b", "2")]
  Node.render (dl (.groups (entries.map fun (k, v) => { term := dt [k], desc := dd [v] })))
    = "<dl><dt>a</dt><dd>1</dd><dt>b</dt><dd>2</dd></dl>"
#guard
  let rows := [["1", "2"], ["3", "4"]]
  Node.render (table (rows.map fun r => tr (r.map fun (c : String) => td [c])))
    = "<table><tr><td>1</td><td>2</td></tr><tr><td>3</td><td>4</td></tr></table>"

-- A list built at run time may mix every category that widens to its own.
def outline (lines : List String) : List (Node .flow) :=
  lines.map fun (l : String) => if l.isEmpty then hr else if l.startsWith "#" then h2 [l] else p [l, br]
#guard Node.render (div (outline ["# A", "", "b"]))
  = "<div><h2># A</h2><hr><p>b<br></p></div>"
#guard
  let items : List (Node .listItemOrScript) :=
    ["a", "b"].map fun (i : String) => li [i]
  Node.render (ul (script { src := "a.js" } :: Whitespace.newline :: items))
    = "<ul><script src=\"a.js\"></script>\n<li>a</li><li>b</li></ul>"

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

/-- `render_wellFormed` covers `lineBreaks`. -/
example : WellFormedHtml .html5
    (Node.render (details (summary ["More"]) [p ["Body"]]) (lineBreaks := true)) :=
  Node.render_wellFormed _ (by decide +kernel) (lineBreaks := true)

-- Order: `<thead>` can't follow `<tbody>`, nor a caption stand among sections.
/--
error: Application type mismatch: The argument
  thead []
has type
  Node HtmlModel.Html.Cat.tableHead
but is expected to have type
  Node HtmlModel.Html.Cat.tableBody
in the application
  List.cons (thead [])
-/
#guard_msgs in
example : Node .flow := table [tbody [], thead []]

/--
error: Application type mismatch: The argument
  [caption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]]
has type
  List (Node HtmlModel.Html.Cat.tableCaption)
but is expected to have type
  optParam TableBody (TableBody.sections [])
in the application
  @table [caption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]]
-/
#guard_msgs in
example : Node .flow := table [caption ["x"]]

-- Categories: each of these is valid only inside its own parent.
/--
error: Application type mismatch: The argument
  dt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.term
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (dt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := div [dt ["x"]]

/--
error: Application type mismatch: The argument
  dd [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.description
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (dd [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := div [dd ["x"]]

/--
error: Application type mismatch: The argument
  caption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.tableCaption
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (caption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := div [caption ["x"]]

/--
error: Application type mismatch: The argument
  summary [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.summary
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (summary [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := div [summary ["x"]]

/--
error: Application type mismatch: The argument
  legend [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.legend
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (legend [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := div [legend ["x"]]

/--
error: Application type mismatch: The argument
  figcaption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.figureCaption
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (figcaption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := div [figcaption ["x"]]

/--
error: Application type mismatch: The argument
  title "x"
has type
  Node HtmlModel.Html.Cat.title
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (title "x")
-/
#guard_msgs in
example : Node .flow := div [title "x"]

/--
error: failed to synthesize instance of type class
  SourceContext HtmlModel.Html.Cat.flow ?m.3

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: invalid {...} notation, expected type is not known
-/
#guard_msgs in
example : Node .flow := div [source { src := "a.mp4" }]

/--
error: Application type mismatch: The argument
  rt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.rubyText
but is expected to have type
  Node HtmlModel.Html.Cat.phrasing
in the application
  List.cons (rt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .phrasing := span [rt ["x"]]

-- `<html>`: `<head>` then `<body>`.
/--
error: Application type mismatch: The argument
  body []
has type
  Node HtmlModel.Html.Cat.body
but is expected to have type
  Node HtmlModel.Html.Cat.head
in the application
  @document (body [])
-/
#guard_msgs in
example : String := document (body []) (head (title "T"))

-- `<details>`: a `<summary>` first, and only there.
/--
error: Application type mismatch: The argument
  p [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.paragraph
but is expected to have type
  Node HtmlModel.Html.Cat.summary
in the application
  @details (p [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := details (p ["x"])

/--
error: Application type mismatch: The argument
  summary [HtmlModel.Web.widen (HtmlModel.Node.text "b") ⋯]
has type
  Node HtmlModel.Html.Cat.summary
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (summary [HtmlModel.Web.widen (HtmlModel.Node.text "b") ⋯])
-/
#guard_msgs in
example : Node .flow := details (summary ["a"]) [summary ["b"]]

-- `<fieldset>`: a `<legend>` only as the optional first child.
/--
error: Application type mismatch: The argument
  legend [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.legend
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (legend [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := fieldset [legend ["x"]]

-- `<figure>`: a `<figcaption>` only first or last.
/--
error: Application type mismatch: The argument
  figcaption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.figureCaption
but is expected to have type
  Node HtmlModel.Html.Cat.flow
in the application
  List.cons (figcaption [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := figure [p ["a"], figcaption ["x"], p ["b"]]

-- `<dl>`: terms before descriptions, and bare groups never mixed with `<div>`s.
/--
error: Type mismatch
  dd [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.description
but is expected to have type
  Node HtmlModel.Html.Cat.term
---
error: Type mismatch
  dt [HtmlModel.Web.widen (HtmlModel.Node.text "y") ⋯]
has type
  Node HtmlModel.Html.Cat.term
but is expected to have type
  Node HtmlModel.Html.Cat.description
-/
#guard_msgs in
example : DlGroup := { term := dd ["x"], desc := dt ["y"] }

/--
error: Application type mismatch: The argument
  dt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.term
but is expected to have type
  Node HtmlModel.Html.Cat.descriptionGroupOrScript
in the application
  List.cons (dt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .flow := dl (.divs [dt ["x"]])

-- `<picture>`: sources, then exactly one `<img>`.
/--
error: Application type mismatch: The argument
  img { src := "a", alt := "a" }
has type
  Node HtmlModel.Html.Cat.image
but is expected to have type
  Node HtmlModel.Html.Cat.pictureSourceOrScript
in the application
  List.cons (img { src := "a", alt := "a" })
-/
#guard_msgs in
example : Node .phrasing := picture [img { src := "a", alt := "a" }] (source)

/--
error: Application type mismatch: The argument
  span []
has type
  Node HtmlModel.Html.Cat.phrasing
but is expected to have type
  Node HtmlModel.Html.Cat.image
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
  Node HtmlModel.Html.Cat.track
but is expected to have type
  Node HtmlModel.Html.Cat.source
in the application
  List.cons (track { src := "a.vtt" })
-/
#guard_msgs in
example : Node .flow := video (.sources [track { src := "a.vtt" }])

/--
error: failed to synthesize instance of type class
  SourceContext HtmlModel.Html.Cat.flow ?m.6

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
---
error: invalid {...} notation, expected type is not known
-/
#guard_msgs in
example : Node .flow := video (.src "a.mp4") [] [source { src := "a.mp4" }]

-- `<ruby>`: annotations aren't base text, and `<rp>` only brackets `<rt>`.
/--
error: Application type mismatch: The argument
  rt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.rubyText
but is expected to have type
  Node HtmlModel.Html.Cat.phrasing
in the application
  List.cons (rt [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .phrasing := ruby ⟨[rt ["x"]], rt ["y"]⟩

/--
error: Application type mismatch: The argument
  rt [HtmlModel.Web.widen (HtmlModel.Node.text "y") ⋯]
has type
  Node HtmlModel.Html.Cat.rubyText
but is expected to have type
  Node HtmlModel.Html.Cat.rubyParenthesis
in the application
  @RubyText.parenthesized (rt [HtmlModel.Web.widen (HtmlModel.Node.text "y") ⋯])
-/
#guard_msgs in
example : Node .phrasing := ruby ⟨["x"], .parenthesized (rt ["y"]) (rt ["y"], rp [")"])⟩

-- `<hgroup>`: exactly one heading.
/--
error: Application type mismatch: The argument
  h2 [HtmlModel.Web.widen (HtmlModel.Node.text "b") ⋯]
has type
  Node HtmlModel.Html.Cat.heading
but is expected to have type
  Node HtmlModel.Html.Cat.paragraphOrScript
in the application
  List.cons (h2 [HtmlModel.Web.widen (HtmlModel.Node.text "b") ⋯])
-/
#guard_msgs in
example : Node .flow := hgroup [] (h1 ["a"]) [h2 ["b"]]

/--
error: Application type mismatch: The argument
  p [HtmlModel.Web.widen (HtmlModel.Node.text "a") ⋯]
has type
  Node HtmlModel.Html.Cat.paragraph
but is expected to have type
  Node HtmlModel.Html.Cat.heading
in the application
  @hgroup [] (p [HtmlModel.Web.widen (HtmlModel.Node.text "a") ⋯])
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
  Node HtmlModel.Html.Cat.listItemOrScript
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
  Node HtmlModel.Html.Cat.tableCellOrScript
in the application
  List.cons "hello"
-/
#guard_msgs in
example : Node .tableRow := tr ["hello"]

/--
error: could not synthesize default value for field 'isWhitespace' of 'Html.Whitespace' using tactics
---
error: Tactic `decide` proved that the proposition
  HtmlModel.isWhitespace "x" = true
is false
-/
#guard_msgs in
example : Whitespace := { val := "x" }

/--
error: failed to synthesize instance of type class
  HtmlModel.Web.Widens HtmlModel.Web.Cat.ws (HtmlModel.Web.Cat.html HtmlModel.Html.Cat.summary)

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
example : Node .flow := details (Node.whitespace .newline)

-- `<summary>` and `<legend>`: heading content, but not other flow content, and `<hgroup>` holds
-- an `h1` to `h6`, not another `<hgroup>`.
/--
error: Application type mismatch: The argument
  p [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]
has type
  Node HtmlModel.Html.Cat.paragraph
but is expected to have type
  Node HtmlModel.Html.Cat.phrasingOrHeading
in the application
  List.cons (p [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
-/
#guard_msgs in
example : Node .summary := summary [p ["x"]]

/--
error: Application type mismatch: The argument
  hgroup [] (h1 [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯])
has type
  Node HtmlModel.Html.Cat.headingGroup
but is expected to have type
  Node HtmlModel.Html.Cat.heading
in the application
  @hgroup [] (hgroup [] (h1 [HtmlModel.Web.widen (HtmlModel.Node.text "x") ⋯]))
-/
#guard_msgs in
example : Node .headingGroup := hgroup [] (hgroup [] (h1 ["x"]))

-- Script-supporting elements: never among a `<colgroup>`'s columns or a media element's
-- sources, and never in place of a required or optional element.
/--
error: Application type mismatch: The argument
  script { src := "a.js" }
has type
  Node HtmlModel.Html.Cat.scriptSupporting
but is expected to have type
  Node HtmlModel.Html.Cat.tableColumn
in the application
  List.cons (script { src := "a.js" })
-/
#guard_msgs in
example : Node .tableColumnGroup := colgroup [col, script { src := "a.js" }]

/--
error: Application type mismatch: The argument
  script { src := "a.js" }
has type
  Node HtmlModel.Html.Cat.scriptSupporting
but is expected to have type
  Node HtmlModel.Html.Cat.source
in the application
  List.cons (script { src := "a.js" })
-/
#guard_msgs in
example : Node .flow := video (.sources [script { src := "a.js" }])

/--
error: Type mismatch
  script { src := "a.js" }
has type
  Node HtmlModel.Html.Cat.scriptSupporting
but is expected to have type
  Node HtmlModel.Html.Cat.term
-/
#guard_msgs in
example : DlGroup := { term := script { src := "a.js" }, desc := dd ["x"] }

/--
error: Application type mismatch: The argument
  script { src := "a.js" }
has type
  Node HtmlModel.Html.Cat.scriptSupporting
but is expected to have type
  Node HtmlModel.Html.Cat.tableHead
in the application
  some (script { src := "a.js" })
-/
#guard_msgs in
example : Node .flow := table (thead := some (script { src := "a.js" }))

-- `<source>`: `srcset` in a `<picture>`, `src` in a media element.
/--
error: `src` is not a field of structure `PictureSourceAttrs`
-/
#guard_msgs in
example : Node .phrasing := picture [source { srcset := "a.png", src := "a.png" }] (img { src := "a.png", alt := "A" })

/--
error: `srcset` is not a field of structure `SourceAttrs`
-/
#guard_msgs in
example : Node .flow := video (.sources [source { src := "a.mp4", srcset := "a.mp4" }])

-- `<noscript>` in `<head>`: only `link` and `meta`.
/--
error: Application type mismatch: The argument
  script { src := "a.js" }
has type
  Node HtmlModel.Html.Cat.scriptSupporting
but is expected to have type
  Node (noscriptContent HtmlModel.Html.Cat.metadata)
in the application
  List.cons (script { src := "a.js" })
-/
#guard_msgs in
example : Node .head := head (title "T") [noscript [script { src := "a.js" }]]

/--
error: Application type mismatch: The argument
  "x"
has type
  String
but is expected to have type
  Node (noscriptContent HtmlModel.Html.Cat.metadata)
in the application
  List.cons "x"
-/
#guard_msgs in
example : Node .head := head (title "T") [noscript ["x"]]

end HtmlTests

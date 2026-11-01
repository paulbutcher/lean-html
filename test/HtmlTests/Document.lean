/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html
meta import Html

namespace HtmlTests

open Html

#guard document (head (title "T") [link { rel := "stylesheet", href := "/style.css" }]) (body [])
  = "<!DOCTYPE html><html><head>" ++
    "<link rel=\"stylesheet\" href=\"/style.css\"><title>T</title></head><body></body></html>"
#guard document
    (head (title "T") [script { src := "/a.js", integrity := "sha384-x", crossorigin := "anonymous" }])
    (body [])
  = "<!DOCTYPE html><html><head>" ++
    "<script src=\"/a.js\" integrity=\"sha384-x\" crossorigin=\"anonymous\"></script>" ++
    "<title>T</title></head><body></body></html>"

-- `base` precedes every other child, whatever order the arguments come in.
#guard document (head (title "T") [link { rel := "icon", href := "i.png" }] (base := base { href := "/" }))
    (body [])
  = "<!DOCTYPE html><html><head><base href=\"/\"><link rel=\"icon\" href=\"i.png\">" ++
    "<title>T</title></head><body></body></html>"

-- A `<head>` needs exactly one `<title>`, which is not `<head>` content.
/--
error: Application type mismatch: The argument
  meta_ [("charset", "utf-8")]
has type
  Node HtmlModel.Html.Cat.phrasingMetadata
but is expected to have type
  Node HtmlModel.Html.Cat.title
in the application
  @head (meta_ [("charset", "utf-8")])
-/
#guard_msgs in
example : Node .head := head (meta_ [("charset", "utf-8")])

/--
error: Application type mismatch: The argument
  title "U"
has type
  Node HtmlModel.Html.Cat.title
but is expected to have type
  Node HtmlModel.Html.Cat.metadata
in the application
  List.cons (title "U")
-/
#guard_msgs in
example : Node .head := head (title "T") [title "U"]

-- Whitespace can't stand in for the title.
/--
error: failed to synthesize instance of type class
  HtmlModel.Web.Widens HtmlModel.Web.Cat.ws (HtmlModel.Web.Cat.html HtmlModel.Html.Cat.title)

Hint: Type class instance resolution failures can be inspected with the `set_option trace.Meta.synthInstance true` command.
-/
#guard_msgs in
example : Node .head := head (Node.whitespace .newline)

end HtmlTests

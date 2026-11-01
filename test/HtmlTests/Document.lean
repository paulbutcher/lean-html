/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html.Document
import Html.Node
import Html.Render
import Html.Tags
meta import Html.Document
meta import Html.Node
meta import Html.Render
meta import Html.Tags

namespace HtmlTests

open Html

#guard document (head (title "T")) (body [])
  = "<!DOCTYPE html><html><head><title>T</title></head><body></body></html>"
#guard document (head (title "T")) (body []) (lang := "en")
  = "<!DOCTYPE html><html lang=\"en\"><head><title>T</title></head><body></body></html>"
#guard document (head (title "T")) (body [p [Node.text "hi"]])
  = "<!DOCTYPE html><html><head><title>T</title></head><body><p>hi</p></body></html>"
#guard document (head (title "T") [meta_ [("charset", "utf-8")]]) (body [])
  = "<!DOCTYPE html><html><head><meta charset=\"utf-8\"><title>T</title></head><body></body></html>"
#guard document (head (title "T") [link { rel := "stylesheet", href := "/style.css" }]) (body [])
  = "<!DOCTYPE html><html><head>" ++
    "<link rel=\"stylesheet\" href=\"/style.css\"><title>T</title></head><body></body></html>"
#guard document (head (title "<script>")) (body [])
  = "<!DOCTYPE html><html><head><title>&lt;script&gt;</title></head><body></body></html>"
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

#guard document (head (title "T")) (body []) (pretty := true)
  = "<!DOCTYPE html>\n<html>\n  <head>\n    <title>T</title>\n  </head>\n  <body></body>\n</html>"
#guard document (head (title "T")) (body [p [Node.text "hi"]]) (pretty := true) (unit := "    ")
  = "<!DOCTYPE html>\n<html>\n    <head>\n        <title>T</title>\n    </head>\n" ++
    "    <body>\n        <p>hi</p>\n    </body>\n</html>"

-- Nothing in `<head>` renders, so each of its children has a line of its own, though `meta` and
-- `link` are phrasing content in the body.
#guard document (head (title "T") [meta_ [("charset", "utf-8")], link { rel := "icon", href := "i.png" }])
    (body [p ["a", meta_ [("itemprop", "x")]]]) (pretty := true)
  = "<!DOCTYPE html>\n<html>\n  <head>\n    <meta charset=\"utf-8\">\n" ++
    "    <link rel=\"icon\" href=\"i.png\">\n    <title>T</title>\n  </head>\n" ++
    "  <body>\n    <p>a<meta itemprop=\"x\"></p>\n  </body>\n</html>"
#guard Node.render (head (title "T") [meta_ [("charset", "utf-8")], link { rel := "icon", href := "i.png" }])
    (lineBreaks := true)
  = "<head>\n<meta charset=\"utf-8\">\n<link rel=\"icon\" href=\"i.png\">\n<title>T</title>\n</head>"

#guard document (head (title "T") [meta_ [("charset", "utf-8")]]) (body []) (dialect := .xhtml)
  = "<!DOCTYPE html><html xmlns=\"http://www.w3.org/1999/xhtml\">" ++
    "<head><meta charset=\"utf-8\" /><title>T</title></head><body></body></html>"
#guard document (head (title "T")) (body []) (lang := "en") (dialect := .xhtml)
  = "<!DOCTYPE html><html xmlns=\"http://www.w3.org/1999/xhtml\" lang=\"en\" xml:lang=\"en\">" ++
    "<head><title>T</title></head><body></body></html>"
#guard document (head (title "T")) (body [input { type := "checkbox", checked := true }])
    (dialect := .xhtml)
  = "<!DOCTYPE html><html xmlns=\"http://www.w3.org/1999/xhtml\"><head><title>T</title></head>" ++
    "<body><input type=\"checkbox\" checked=\"checked\" /></body></html>"

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

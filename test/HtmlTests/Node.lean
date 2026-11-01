/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html
meta import Html

namespace HtmlTests

open Html

-- String literals coerce directly to a text node (no `Node.text` needed).
#guard Node.render (p ["hi"]) = "<p>hi</p>"

#guard Node.render (input { disabled := true }) = "<input type=\"text\" disabled>"
#guard Node.render (input { disabled := true }) (dialect := .xhtml)
  = "<input type=\"text\" disabled=\"disabled\" />"
#guard Node.render (details (summary []) [] { open_ := true }) (dialect := .xhtml)
  = "<details open=\"open\"><summary></summary></details>"

-- A transparent element laid out inline holds only phrasing content.
#guard Node.renderPretty (div [a { href := "/" } ["x"], p []])
  = "<div>\n  <a href=\"/\">x</a>\n  <p></p>\n</div>"

end HtmlTests

/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

public import Html.Node
public import Html.Escape
public import Html.Attrs
public import Html.Tags

public section

namespace Html

/-- Prepends `<!DOCTYPE html>` and wraps `children` in a single `<html>` element.
`pretty` selects indented output, with `unit` repeated per indentation level;
`selfClosingVoid` selects XHTML-style void tags (`<br />`) over HTML5 style
(`<br>`). -/
def document (children : List (Node .flow))
    (lang : Option String := none) (pretty : Bool := false) (unit : String := "  ")
    (selfClosingVoid : Bool := false) : String :=
  let attrs : Attrs := optAttr "lang" lang
  let htmlNode : Node .flow := Node.element .flow "html" children attrs
  if pretty then "<!DOCTYPE html>\n" ++ Node.renderPretty htmlNode unit selfClosingVoid
  else "<!DOCTYPE html>" ++ Node.render htmlNode selfClosingVoid

end Html

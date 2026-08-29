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
`dialect` selects HTML5 or XML serialization. The doctype is valid in both, but
XML also needs the namespace declared on the root element, and takes the
language from `xml:lang`, so `.xhtml` emits both spellings of a given `lang`. -/
def document (children : List (Node .flow))
    (lang : Option String := none) (pretty : Bool := false) (unit : String := "  ")
    (dialect : Dialect := .html5) : String :=
  let attrs : Attrs :=
    match dialect with
    | .html5 => optAttr "lang" lang
    | .xhtml =>
      reqAttr "xmlns" "http://www.w3.org/1999/xhtml" ++ optAttr "lang" lang
        ++ optAttr "xml:lang" lang
  let htmlNode : Node .flow := Node.element .flow "html" children attrs
  if pretty then "<!DOCTYPE html>\n" ++ Node.renderPretty htmlNode unit dialect
  else "<!DOCTYPE html>" ++ Node.render htmlNode dialect

end Html

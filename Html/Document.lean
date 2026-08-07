import Html.Node
import Html.Escape
import Html.Attrs
import Html.Tags

namespace Html

/-- Prepends `<!DOCTYPE html>` and wraps `children` in a single `<html>` element.
`pretty` selects indented
`unit` is the string repeated per indentation level (default two spaces) -/
def document (children : List (Node .flow))
    (lang : Option String := none) (pretty : Bool := false) (unit : String := "  ") : String :=
  let attrs : Attrs := optAttr "lang" lang
  let htmlNode : Node .flow := Node.element .flow "html" children attrs
  if pretty then "<!DOCTYPE html>\n" ++ Node.renderPretty htmlNode unit
  else "<!DOCTYPE html>" ++ Node.render htmlNode

end Html

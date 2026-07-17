namespace Html

/-- Escapes `&`, `<`, `>`, `"` -/
def escapeChar (c : Char) : String :=
  match c with
  | '&' => "&amp;" -- Must be first
  | '<' => "&lt;"
  | '>' => "&gt;"
  | '"' => "&quot;"
  | c    => String.singleton c

/-- Escape a whole string. -/
def escape (s : String) : String :=
  String.join (s.toList.map escapeChar)

/-- Render one attribute as `name="escaped value"`, with a leading space
so callers can concatenate these directly after a tag name. -/
def renderAttr (name value : String) : String :=
  s!" {name}=\"{escape value}\""

end Html

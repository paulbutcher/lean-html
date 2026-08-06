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

/-- Characters allowed in a sanitized attribute name: ASCII letters,
digits, `-`, `_`, `:`, `.` -- enough for real-world names (`data-x`,
`aria-label`, `v-on:click`) while excluding anything (whitespace, `=`,
`<`, `>`, quotes) that would let a name break out of the tag it's
rendered into. -/
def isAttrNameChar (c : Char) : Bool :=
  c.isAlphanum || c == '-' || c == '_' || c == ':' || c == '.'

def sanitizeAttrNameChar (c : Char) : Char :=
  if isAttrNameChar c then c else '_'

/-- Coerce an arbitrary string into a safe attribute name by replacing
every disallowed character with `_`, falling back to `_` itself when that
leaves nothing. Mirrors `escape`'s role for attribute *values*: callers
can pass untrusted strings as `rawAttrs` names (`Html/Attrs.lean`'s
`renderRawAttrs`) and still get well-formed output. -/
def sanitizeAttrName (s : String) : String :=
  let sanitized := s.map sanitizeAttrNameChar
  if sanitized.isEmpty then "_" else sanitized

end Html

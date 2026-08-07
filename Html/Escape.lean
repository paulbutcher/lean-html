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

/-- One rendered attribute: a `name="value"` pair, or a bare boolean flag
(`name`, no value). Names and values are stored *unsanitized/unescaped* --
`Attrs.render` is the only place that happens. -/
inductive AttrFragment where
  | value (name val : String)
  | flag (name : String)

/-- An attribute list, in emission order. This is the only type
`Html/Node.lean`'s element constructors accept for attributes -- unlike a
bare `attrsStr : String`, an `Attrs` value can't smuggle in an unescaped
`<`/`>`/`"` no matter what strings its fragments carry, because turning it
into a string always goes through `Attrs.render` below. -/
abbrev Attrs := List AttrFragment

/-- Render an attribute list to a string, sanitizing every name and
escaping every value as it goes. This is the single choke point that makes
any `Attrs` value safe to splice directly after a tag name. -/
def Attrs.render (attrs : Attrs) : String :=
  String.join (attrs.map fun
    | .value name val => renderAttr (sanitizeAttrName name) val
    | .flag name => s!" {sanitizeAttrName name}")

/-- A single required `name="value"` attribute. -/
def reqAttr (name value : String) : Attrs := [.value name value]

/-- A single `name="value"` attribute when present, nothing otherwise. -/
def optAttr (name : String) : Option String → Attrs
  | none => []
  | some v => [.value name v]

/-- A single bare `name` flag when `true`, nothing otherwise. -/
def flagAttr (name : String) (b : Bool) : Attrs :=
  if b then [.flag name] else []

/-- Render arbitrary `(name, value)` pairs: values escaped, names sanitized
(see `sanitizeAttrName`) so a name containing e.g. a space or `"` can't
break out of the tag it's rendered into. -/
def renderRawAttrs (attrs : List (String × String)) : String :=
  Attrs.render (attrs.map fun (n, v) => .value n v)

end Html

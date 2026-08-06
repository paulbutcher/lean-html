import Html.Escape

namespace Html

/-- Lets `{ id := "x" }` elaborate directly against an `Option String`
field without writing `some "x"` -/
scoped instance : Coe String (Option String) := ⟨some⟩

/-- Render a boolean attribute: the bare attribute name when `true`,
absent entirely when `false` -/
def renderBoolAttr (name : String) : Bool → String
  | true => s!" {name}"
  | false => ""

/-- Render one optional string-valued attribute: escaped and
double-quote-delimited when present, empty when absent. -/
private def renderOpt (name : String) : Option String → String
  | none => ""
  | some v => renderAttr name v

/-- Render arbitrary `(name, value)` pairs: values escaped, names sanitized
(see `sanitizeAttrName`) so a name containing e.g. a space or `"` can't
break out of the tag it's rendered into. -/
def renderRawAttrs (attrs : List (String × String)) : String :=
  String.join (attrs.map (fun (n, v) => renderAttr (sanitizeAttrName n) v))

/-- Global attributes, valid on any element. `class_` (not `class`, a
Lean keyword) renders as the `class` attribute. Every per-element record
below `extends` this, so a single attrs argument carries both an
element's own attributes and these global ones. -/
structure HtmlAttrs where
  id : Option String := none
  class_ : Option String := none
  style : Option String := none
  title : Option String := none
  lang : Option String := none
  dir : Option String := none

def HtmlAttrs.render (a : HtmlAttrs) : String :=
  renderOpt "id" a.id ++ renderOpt "class" a.class_ ++ renderOpt "style" a.style ++
    renderOpt "title" a.title ++ renderOpt "lang" a.lang ++ renderOpt "dir" a.dir

/-- `<a>` -/
structure AAttrs extends HtmlAttrs where
  href : String
  target : Option String := none
  rel : Option String := none

def AAttrs.render (a : AAttrs) : String :=
  renderAttr "href" a.href ++ renderOpt "target" a.target ++ renderOpt "rel" a.rel ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<img>` -/
structure ImgAttrs extends HtmlAttrs where
  src : String
  alt : String

def ImgAttrs.render (a : ImgAttrs) : String :=
  renderAttr "src" a.src ++ renderAttr "alt" a.alt ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<input>` -/
structure InputAttrs extends HtmlAttrs where
  type : String := "text"
  name : Option String := none
  value : Option String := none
  placeholder : Option String := none
  disabled : Bool := false
  checked : Bool := false
  required : Bool := false
  readonly : Bool := false

def InputAttrs.render (a : InputAttrs) : String :=
  renderAttr "type" a.type ++ renderOpt "name" a.name ++ renderOpt "value" a.value ++
    renderOpt "placeholder" a.placeholder ++ renderBoolAttr "disabled" a.disabled ++
    renderBoolAttr "checked" a.checked ++ renderBoolAttr "required" a.required ++
    renderBoolAttr "readonly" a.readonly ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<script>` -/
structure ScriptAttrs extends HtmlAttrs where
  src : String
  integrity : Option String := none
  crossorigin : Option String := none

def ScriptAttrs.render (a : ScriptAttrs) : String :=
  renderAttr "src" a.src ++ renderOpt "integrity" a.integrity ++ renderOpt "crossorigin" a.crossorigin ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<link>` -/
structure LinkAttrs extends HtmlAttrs where
  rel : String
  href : String

def LinkAttrs.render (a : LinkAttrs) : String :=
  renderAttr "rel" a.rel ++ renderAttr "href" a.href ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<q>` -/
structure QAttrs extends HtmlAttrs where
  cite : Option String := none

def QAttrs.render (a : QAttrs) : String :=
  renderOpt "cite" a.cite ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<time>` -/
structure TimeAttrs extends HtmlAttrs where
  datetime : Option String := none

def TimeAttrs.render (a : TimeAttrs) : String :=
  renderOpt "datetime" a.datetime ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<data>` `value` is required. -/
structure DataAttrs extends HtmlAttrs where
  value : String

def DataAttrs.render (a : DataAttrs) : String :=
  renderAttr "value" a.value ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<ins>`/`<del>` -/
structure InsDelAttrs extends HtmlAttrs where
  cite : Option String := none
  datetime : Option String := none

def InsDelAttrs.render (a : InsDelAttrs) : String :=
  renderOpt "cite" a.cite ++ renderOpt "datetime" a.datetime ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<col>` -/
structure ColAttrs extends HtmlAttrs where
  span : Option String := none

def ColAttrs.render (a : ColAttrs) : String :=
  renderOpt "span" a.span ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<fieldset>` -/
structure FieldsetAttrs extends HtmlAttrs where
  disabled : Bool := false
  name : Option String := none

def FieldsetAttrs.render (a : FieldsetAttrs) : String :=
  renderBoolAttr "disabled" a.disabled ++ renderOpt "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<optgroup>` -/
structure OptgroupAttrs extends HtmlAttrs where
  label : String
  disabled : Bool := false

def OptgroupAttrs.render (a : OptgroupAttrs) : String :=
  renderAttr "label" a.label ++ renderBoolAttr "disabled" a.disabled ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<output>`
`for_` (trailing underscore -- `for` is a Lean keyword). -/
structure OutputAttrs extends HtmlAttrs where
  for_ : Option String := none
  name : Option String := none

def OutputAttrs.render (a : OutputAttrs) : String :=
  renderOpt "for" a.for_ ++ renderOpt "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<progress>` -/
structure ProgressAttrs extends HtmlAttrs where
  value : Option String := none
  max : Option String := none

def ProgressAttrs.render (a : ProgressAttrs) : String :=
  renderOpt "value" a.value ++ renderOpt "max" a.max ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<meter>` -/
structure MeterAttrs extends HtmlAttrs where
  value : Option String := none
  min : Option String := none
  max : Option String := none
  low : Option String := none
  high : Option String := none
  optimum : Option String := none

def MeterAttrs.render (a : MeterAttrs) : String :=
  renderOpt "value" a.value ++ renderOpt "min" a.min ++ renderOpt "max" a.max ++
    renderOpt "low" a.low ++ renderOpt "high" a.high ++ renderOpt "optimum" a.optimum ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<details>`/`<dialog>`
`open_` (trailing underscore -- `open` is a Lean keyword). -/
structure OpenAttrs extends HtmlAttrs where
  open_ : Bool := false

def OpenAttrs.render (a : OpenAttrs) : String :=
  renderBoolAttr "open" a.open_ ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<base>` -/
structure BaseAttrs extends HtmlAttrs where
  href : Option String := none
  target : Option String := none

def BaseAttrs.render (a : BaseAttrs) : String :=
  renderOpt "href" a.href ++ renderOpt "target" a.target ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<canvas>` -/
structure CanvasAttrs extends HtmlAttrs where
  width : Option String := none
  height : Option String := none

def CanvasAttrs.render (a : CanvasAttrs) : String :=
  renderOpt "width" a.width ++ renderOpt "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<slot>` -/
structure SlotAttrs extends HtmlAttrs where
  name : Option String := none

def SlotAttrs.render (a : SlotAttrs) : String :=
  renderOpt "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<source>`
Dual-purpose in the HTML spec:
- inside `<picture>` it's `srcset`/`type`/`media` (no `src`)
- inside `<video>`/`<audio>` it's `src`/`type` (no `srcset`)
not distinguished here. -/
structure SourceAttrs extends HtmlAttrs where
  src : Option String := none
  srcset : Option String := none
  type : Option String := none
  media : Option String := none

def SourceAttrs.render (a : SourceAttrs) : String :=
  renderOpt "src" a.src ++ renderOpt "srcset" a.srcset ++ renderOpt "type" a.type ++
    renderOpt "media" a.media ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<track>` -/
structure TrackAttrs extends HtmlAttrs where
  src : String
  kind : Option String := none
  srclang : Option String := none
  label : Option String := none
  default : Bool := false

def TrackAttrs.render (a : TrackAttrs) : String :=
  renderAttr "src" a.src ++ renderOpt "kind" a.kind ++ renderOpt "srclang" a.srclang ++
    renderOpt "label" a.label ++ renderBoolAttr "default" a.default ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<iframe>`
`title` isn't declared here -- it's the same attribute as `HtmlAttrs.title`
(strongly recommended for `<iframe>` specifically, for accessibility), so
it's inherited rather than redeclared. -/
structure IframeAttrs extends HtmlAttrs where
  src : String
  width : Option String := none
  height : Option String := none

def IframeAttrs.render (a : IframeAttrs) : String :=
  renderAttr "src" a.src ++ renderOpt "width" a.width ++
    renderOpt "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<embed>` -/
structure EmbedAttrs extends HtmlAttrs where
  src : Option String := none
  type : Option String := none
  width : Option String := none
  height : Option String := none

def EmbedAttrs.render (a : EmbedAttrs) : String :=
  renderOpt "src" a.src ++ renderOpt "type" a.type ++ renderOpt "width" a.width ++
    renderOpt "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<object>` -/
structure ObjectAttrs extends HtmlAttrs where
  data : Option String := none
  type : Option String := none
  width : Option String := none
  height : Option String := none

def ObjectAttrs.render (a : ObjectAttrs) : String :=
  renderOpt "data" a.data ++ renderOpt "type" a.type ++ renderOpt "width" a.width ++
    renderOpt "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<video>` -/
structure VideoAttrs extends HtmlAttrs where
  src : Option String := none
  poster : Option String := none
  controls : Bool := false
  autoplay : Bool := false
  loop : Bool := false
  muted : Bool := false
  width : Option String := none
  height : Option String := none

def VideoAttrs.render (a : VideoAttrs) : String :=
  renderOpt "src" a.src ++ renderOpt "poster" a.poster ++ renderBoolAttr "controls" a.controls ++
    renderBoolAttr "autoplay" a.autoplay ++ renderBoolAttr "loop" a.loop ++
    renderBoolAttr "muted" a.muted ++ renderOpt "width" a.width ++ renderOpt "height" a.height ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<audio>` -/
structure AudioAttrs extends HtmlAttrs where
  src : Option String := none
  controls : Bool := false
  autoplay : Bool := false
  loop : Bool := false
  muted : Bool := false

def AudioAttrs.render (a : AudioAttrs) : String :=
  renderOpt "src" a.src ++ renderBoolAttr "controls" a.controls ++
    renderBoolAttr "autoplay" a.autoplay ++ renderBoolAttr "loop" a.loop ++
    renderBoolAttr "muted" a.muted ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<map>` -/
structure MapAttrs extends HtmlAttrs where
  name : String

def MapAttrs.render (a : MapAttrs) : String :=
  renderAttr "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<area>` -/
structure AreaAttrs extends HtmlAttrs where
  href : Option String := none
  alt : Option String := none
  shape : Option String := none
  coords : Option String := none
  target : Option String := none

def AreaAttrs.render (a : AreaAttrs) : String :=
  renderOpt "href" a.href ++ renderOpt "alt" a.alt ++ renderOpt "shape" a.shape ++
    renderOpt "coords" a.coords ++ renderOpt "target" a.target ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<ol>` -/
structure OlAttrs extends HtmlAttrs where
  reversed : Bool := false
  start : Option String := none
  type : Option String := none

def OlAttrs.render (a : OlAttrs) : String :=
  renderBoolAttr "reversed" a.reversed ++ renderOpt "start" a.start ++ renderOpt "type" a.type ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<li>` -/
structure LiAttrs extends HtmlAttrs where
  value : Option String := none

def LiAttrs.render (a : LiAttrs) : String :=
  renderOpt "value" a.value ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<blockquote>` -/
structure BlockquoteAttrs extends HtmlAttrs where
  cite : Option String := none

def BlockquoteAttrs.render (a : BlockquoteAttrs) : String :=
  renderOpt "cite" a.cite ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<form>` -/
structure FormAttrs extends HtmlAttrs where
  action : Option String := none
  method : Option String := none

def FormAttrs.render (a : FormAttrs) : String :=
  renderOpt "action" a.action ++ renderOpt "method" a.method ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<label>`
`for_` (trailing underscore -- `for` is a Lean keyword). -/
structure LabelAttrs extends HtmlAttrs where
  for_ : Option String := none

def LabelAttrs.render (a : LabelAttrs) : String :=
  renderOpt "for" a.for_ ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<textarea>` -/
structure TextareaAttrs extends HtmlAttrs where
  name : Option String := none
  placeholder : Option String := none
  rows : Option String := none
  cols : Option String := none
  disabled : Bool := false
  readonly : Bool := false
  required : Bool := false
  autofocus : Bool := false

def TextareaAttrs.render (a : TextareaAttrs) : String :=
  renderOpt "name" a.name ++ renderOpt "placeholder" a.placeholder ++ renderOpt "rows" a.rows ++
    renderOpt "cols" a.cols ++ renderBoolAttr "disabled" a.disabled ++
    renderBoolAttr "readonly" a.readonly ++ renderBoolAttr "required" a.required ++
    renderBoolAttr "autofocus" a.autofocus ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<option>` -/
structure OptionAttrs extends HtmlAttrs where
  value : Option String := none
  selected : Bool := false
  disabled : Bool := false
  label : Option String := none

def OptionAttrs.render (a : OptionAttrs) : String :=
  renderOpt "value" a.value ++ renderBoolAttr "selected" a.selected ++
    renderBoolAttr "disabled" a.disabled ++ renderOpt "label" a.label ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<select>` -/
structure SelectAttrs extends HtmlAttrs where
  name : Option String := none
  multiple : Bool := false
  disabled : Bool := false
  required : Bool := false
  size : Option String := none

def SelectAttrs.render (a : SelectAttrs) : String :=
  renderOpt "name" a.name ++ renderBoolAttr "multiple" a.multiple ++
    renderBoolAttr "disabled" a.disabled ++ renderBoolAttr "required" a.required ++
    renderOpt "size" a.size ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<button>` -/
structure ButtonAttrs extends HtmlAttrs where
  type : String := "submit"
  name : Option String := none
  value : Option String := none
  disabled : Bool := false

def ButtonAttrs.render (a : ButtonAttrs) : String :=
  renderAttr "type" a.type ++ renderOpt "name" a.name ++ renderOpt "value" a.value ++
    renderBoolAttr "disabled" a.disabled ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<th>` -/
structure ThAttrs extends HtmlAttrs where
  colspan : Option String := none
  rowspan : Option String := none
  scope : Option String := none

def ThAttrs.render (a : ThAttrs) : String :=
  renderOpt "colspan" a.colspan ++ renderOpt "rowspan" a.rowspan ++ renderOpt "scope" a.scope ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<td>` -/
structure TdAttrs extends HtmlAttrs where
  colspan : Option String := none
  rowspan : Option String := none

def TdAttrs.render (a : TdAttrs) : String :=
  renderOpt "colspan" a.colspan ++ renderOpt "rowspan" a.rowspan ++ HtmlAttrs.render a.toHtmlAttrs

end Html

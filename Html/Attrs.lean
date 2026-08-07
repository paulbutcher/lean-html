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

def HtmlAttrs.render (a : HtmlAttrs) : Attrs :=
  optAttr "id" a.id ++ optAttr "class" a.class_ ++ optAttr "style" a.style ++
    optAttr "title" a.title ++ optAttr "lang" a.lang ++ optAttr "dir" a.dir

/-- `<a>` -/
structure AAttrs extends HtmlAttrs where
  href : String
  target : Option String := none
  rel : Option String := none

def AAttrs.render (a : AAttrs) : Attrs :=
  reqAttr "href" a.href ++ optAttr "target" a.target ++ optAttr "rel" a.rel ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<img>` -/
structure ImgAttrs extends HtmlAttrs where
  src : String
  alt : String

def ImgAttrs.render (a : ImgAttrs) : Attrs :=
  reqAttr "src" a.src ++ reqAttr "alt" a.alt ++ HtmlAttrs.render a.toHtmlAttrs

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

def InputAttrs.render (a : InputAttrs) : Attrs :=
  reqAttr "type" a.type ++ optAttr "name" a.name ++ optAttr "value" a.value ++
    optAttr "placeholder" a.placeholder ++ flagAttr "disabled" a.disabled ++
    flagAttr "checked" a.checked ++ flagAttr "required" a.required ++
    flagAttr "readonly" a.readonly ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<script>` -/
structure ScriptAttrs extends HtmlAttrs where
  src : String
  integrity : Option String := none
  crossorigin : Option String := none

def ScriptAttrs.render (a : ScriptAttrs) : Attrs :=
  reqAttr "src" a.src ++ optAttr "integrity" a.integrity ++ optAttr "crossorigin" a.crossorigin ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<link>` -/
structure LinkAttrs extends HtmlAttrs where
  rel : String
  href : String

def LinkAttrs.render (a : LinkAttrs) : Attrs :=
  reqAttr "rel" a.rel ++ reqAttr "href" a.href ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<q>` -/
structure QAttrs extends HtmlAttrs where
  cite : Option String := none

def QAttrs.render (a : QAttrs) : Attrs :=
  optAttr "cite" a.cite ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<time>` -/
structure TimeAttrs extends HtmlAttrs where
  datetime : Option String := none

def TimeAttrs.render (a : TimeAttrs) : Attrs :=
  optAttr "datetime" a.datetime ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<data>` `value` is required. -/
structure DataAttrs extends HtmlAttrs where
  value : String

def DataAttrs.render (a : DataAttrs) : Attrs :=
  reqAttr "value" a.value ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<ins>`/`<del>` -/
structure InsDelAttrs extends HtmlAttrs where
  cite : Option String := none
  datetime : Option String := none

def InsDelAttrs.render (a : InsDelAttrs) : Attrs :=
  optAttr "cite" a.cite ++ optAttr "datetime" a.datetime ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<col>` -/
structure ColAttrs extends HtmlAttrs where
  span : Option String := none

def ColAttrs.render (a : ColAttrs) : Attrs :=
  optAttr "span" a.span ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<fieldset>` -/
structure FieldsetAttrs extends HtmlAttrs where
  disabled : Bool := false
  name : Option String := none

def FieldsetAttrs.render (a : FieldsetAttrs) : Attrs :=
  flagAttr "disabled" a.disabled ++ optAttr "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<optgroup>` -/
structure OptgroupAttrs extends HtmlAttrs where
  label : String
  disabled : Bool := false

def OptgroupAttrs.render (a : OptgroupAttrs) : Attrs :=
  reqAttr "label" a.label ++ flagAttr "disabled" a.disabled ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<output>`
`for_` (trailing underscore -- `for` is a Lean keyword). -/
structure OutputAttrs extends HtmlAttrs where
  for_ : Option String := none
  name : Option String := none

def OutputAttrs.render (a : OutputAttrs) : Attrs :=
  optAttr "for" a.for_ ++ optAttr "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<progress>` -/
structure ProgressAttrs extends HtmlAttrs where
  value : Option String := none
  max : Option String := none

def ProgressAttrs.render (a : ProgressAttrs) : Attrs :=
  optAttr "value" a.value ++ optAttr "max" a.max ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<meter>` -/
structure MeterAttrs extends HtmlAttrs where
  value : Option String := none
  min : Option String := none
  max : Option String := none
  low : Option String := none
  high : Option String := none
  optimum : Option String := none

def MeterAttrs.render (a : MeterAttrs) : Attrs :=
  optAttr "value" a.value ++ optAttr "min" a.min ++ optAttr "max" a.max ++
    optAttr "low" a.low ++ optAttr "high" a.high ++ optAttr "optimum" a.optimum ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<details>`/`<dialog>`
`open_` (trailing underscore -- `open` is a Lean keyword). -/
structure OpenAttrs extends HtmlAttrs where
  open_ : Bool := false

def OpenAttrs.render (a : OpenAttrs) : Attrs :=
  flagAttr "open" a.open_ ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<base>` -/
structure BaseAttrs extends HtmlAttrs where
  href : Option String := none
  target : Option String := none

def BaseAttrs.render (a : BaseAttrs) : Attrs :=
  optAttr "href" a.href ++ optAttr "target" a.target ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<canvas>` -/
structure CanvasAttrs extends HtmlAttrs where
  width : Option String := none
  height : Option String := none

def CanvasAttrs.render (a : CanvasAttrs) : Attrs :=
  optAttr "width" a.width ++ optAttr "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<slot>` -/
structure SlotAttrs extends HtmlAttrs where
  name : Option String := none

def SlotAttrs.render (a : SlotAttrs) : Attrs :=
  optAttr "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

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

def SourceAttrs.render (a : SourceAttrs) : Attrs :=
  optAttr "src" a.src ++ optAttr "srcset" a.srcset ++ optAttr "type" a.type ++
    optAttr "media" a.media ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<track>` -/
structure TrackAttrs extends HtmlAttrs where
  src : String
  kind : Option String := none
  srclang : Option String := none
  label : Option String := none
  default : Bool := false

def TrackAttrs.render (a : TrackAttrs) : Attrs :=
  reqAttr "src" a.src ++ optAttr "kind" a.kind ++ optAttr "srclang" a.srclang ++
    optAttr "label" a.label ++ flagAttr "default" a.default ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<iframe>`
`title` isn't declared here -- it's the same attribute as `HtmlAttrs.title`
(strongly recommended for `<iframe>` specifically, for accessibility), so
it's inherited rather than redeclared. -/
structure IframeAttrs extends HtmlAttrs where
  src : String
  width : Option String := none
  height : Option String := none

def IframeAttrs.render (a : IframeAttrs) : Attrs :=
  reqAttr "src" a.src ++ optAttr "width" a.width ++
    optAttr "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<embed>` -/
structure EmbedAttrs extends HtmlAttrs where
  src : Option String := none
  type : Option String := none
  width : Option String := none
  height : Option String := none

def EmbedAttrs.render (a : EmbedAttrs) : Attrs :=
  optAttr "src" a.src ++ optAttr "type" a.type ++ optAttr "width" a.width ++
    optAttr "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<object>` -/
structure ObjectAttrs extends HtmlAttrs where
  data : Option String := none
  type : Option String := none
  width : Option String := none
  height : Option String := none

def ObjectAttrs.render (a : ObjectAttrs) : Attrs :=
  optAttr "data" a.data ++ optAttr "type" a.type ++ optAttr "width" a.width ++
    optAttr "height" a.height ++ HtmlAttrs.render a.toHtmlAttrs

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

def VideoAttrs.render (a : VideoAttrs) : Attrs :=
  optAttr "src" a.src ++ optAttr "poster" a.poster ++ flagAttr "controls" a.controls ++
    flagAttr "autoplay" a.autoplay ++ flagAttr "loop" a.loop ++
    flagAttr "muted" a.muted ++ optAttr "width" a.width ++ optAttr "height" a.height ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<audio>` -/
structure AudioAttrs extends HtmlAttrs where
  src : Option String := none
  controls : Bool := false
  autoplay : Bool := false
  loop : Bool := false
  muted : Bool := false

def AudioAttrs.render (a : AudioAttrs) : Attrs :=
  optAttr "src" a.src ++ flagAttr "controls" a.controls ++
    flagAttr "autoplay" a.autoplay ++ flagAttr "loop" a.loop ++
    flagAttr "muted" a.muted ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<map>` -/
structure MapAttrs extends HtmlAttrs where
  name : String

def MapAttrs.render (a : MapAttrs) : Attrs :=
  reqAttr "name" a.name ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<area>` -/
structure AreaAttrs extends HtmlAttrs where
  href : Option String := none
  alt : Option String := none
  shape : Option String := none
  coords : Option String := none
  target : Option String := none

def AreaAttrs.render (a : AreaAttrs) : Attrs :=
  optAttr "href" a.href ++ optAttr "alt" a.alt ++ optAttr "shape" a.shape ++
    optAttr "coords" a.coords ++ optAttr "target" a.target ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<ol>` -/
structure OlAttrs extends HtmlAttrs where
  reversed : Bool := false
  start : Option String := none
  type : Option String := none

def OlAttrs.render (a : OlAttrs) : Attrs :=
  flagAttr "reversed" a.reversed ++ optAttr "start" a.start ++ optAttr "type" a.type ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<li>` -/
structure LiAttrs extends HtmlAttrs where
  value : Option String := none

def LiAttrs.render (a : LiAttrs) : Attrs :=
  optAttr "value" a.value ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<blockquote>` -/
structure BlockquoteAttrs extends HtmlAttrs where
  cite : Option String := none

def BlockquoteAttrs.render (a : BlockquoteAttrs) : Attrs :=
  optAttr "cite" a.cite ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<form>` -/
structure FormAttrs extends HtmlAttrs where
  action : Option String := none
  method : Option String := none

def FormAttrs.render (a : FormAttrs) : Attrs :=
  optAttr "action" a.action ++ optAttr "method" a.method ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<label>`
`for_` (trailing underscore -- `for` is a Lean keyword). -/
structure LabelAttrs extends HtmlAttrs where
  for_ : Option String := none

def LabelAttrs.render (a : LabelAttrs) : Attrs :=
  optAttr "for" a.for_ ++ HtmlAttrs.render a.toHtmlAttrs

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

def TextareaAttrs.render (a : TextareaAttrs) : Attrs :=
  optAttr "name" a.name ++ optAttr "placeholder" a.placeholder ++ optAttr "rows" a.rows ++
    optAttr "cols" a.cols ++ flagAttr "disabled" a.disabled ++
    flagAttr "readonly" a.readonly ++ flagAttr "required" a.required ++
    flagAttr "autofocus" a.autofocus ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<option>` -/
structure OptionAttrs extends HtmlAttrs where
  value : Option String := none
  selected : Bool := false
  disabled : Bool := false
  label : Option String := none

def OptionAttrs.render (a : OptionAttrs) : Attrs :=
  optAttr "value" a.value ++ flagAttr "selected" a.selected ++
    flagAttr "disabled" a.disabled ++ optAttr "label" a.label ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<select>` -/
structure SelectAttrs extends HtmlAttrs where
  name : Option String := none
  multiple : Bool := false
  disabled : Bool := false
  required : Bool := false
  size : Option String := none

def SelectAttrs.render (a : SelectAttrs) : Attrs :=
  optAttr "name" a.name ++ flagAttr "multiple" a.multiple ++
    flagAttr "disabled" a.disabled ++ flagAttr "required" a.required ++
    optAttr "size" a.size ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<button>` -/
structure ButtonAttrs extends HtmlAttrs where
  type : String := "submit"
  name : Option String := none
  value : Option String := none
  disabled : Bool := false

def ButtonAttrs.render (a : ButtonAttrs) : Attrs :=
  reqAttr "type" a.type ++ optAttr "name" a.name ++ optAttr "value" a.value ++
    flagAttr "disabled" a.disabled ++ HtmlAttrs.render a.toHtmlAttrs

/-- `<th>` -/
structure ThAttrs extends HtmlAttrs where
  colspan : Option String := none
  rowspan : Option String := none
  scope : Option String := none

def ThAttrs.render (a : ThAttrs) : Attrs :=
  optAttr "colspan" a.colspan ++ optAttr "rowspan" a.rowspan ++ optAttr "scope" a.scope ++
    HtmlAttrs.render a.toHtmlAttrs

/-- `<td>` -/
structure TdAttrs extends HtmlAttrs where
  colspan : Option String := none
  rowspan : Option String := none

def TdAttrs.render (a : TdAttrs) : Attrs :=
  optAttr "colspan" a.colspan ++ optAttr "rowspan" a.rowspan ++ HtmlAttrs.render a.toHtmlAttrs

end Html

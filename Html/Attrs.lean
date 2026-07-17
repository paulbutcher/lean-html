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

/-- Render arbitrary `(name, value)` pairs verbatim: values escaped, names
*not* validated. -/
def renderRawAttrs (attrs : List (String × String)) : String :=
  String.join (attrs.map (fun (n, v) => renderAttr n v))

/-- Global attributes, valid on any element. `class_` (not `class`, a
Lean keyword) renders as the `class` attribute. -/
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
structure AAttrs where
  href : String
  target : Option String := none
  rel : Option String := none

def AAttrs.render (a : AAttrs) : String :=
  renderAttr "href" a.href ++ renderOpt "target" a.target ++ renderOpt "rel" a.rel

/-- `<img>` -/
structure ImgAttrs where
  src : String
  alt : String

def ImgAttrs.render (a : ImgAttrs) : String :=
  renderAttr "src" a.src ++ renderAttr "alt" a.alt

/-- `<input>` -/
structure InputAttrs where
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
    renderBoolAttr "readonly" a.readonly

/-- `<script>` -/
structure ScriptAttrs where
  src : String
  integrity : Option String := none
  crossorigin : Option String := none

def ScriptAttrs.render (a : ScriptAttrs) : String :=
  renderAttr "src" a.src ++ renderOpt "integrity" a.integrity ++ renderOpt "crossorigin" a.crossorigin

/-- `<link>` -/
structure LinkAttrs where
  rel : String
  href : String

def LinkAttrs.render (a : LinkAttrs) : String :=
  renderAttr "rel" a.rel ++ renderAttr "href" a.href

/-- `<q>` -/
structure QAttrs where
  cite : Option String := none

def QAttrs.render (a : QAttrs) : String :=
  renderOpt "cite" a.cite

/-- `<time>` -/
structure TimeAttrs where
  datetime : Option String := none

def TimeAttrs.render (a : TimeAttrs) : String :=
  renderOpt "datetime" a.datetime

/-- `<data>` `value` is required. -/
structure DataAttrs where
  value : String

def DataAttrs.render (a : DataAttrs) : String :=
  renderAttr "value" a.value

/-- `<ins>`/`<del>` -/
structure InsDelAttrs where
  cite : Option String := none
  datetime : Option String := none

def InsDelAttrs.render (a : InsDelAttrs) : String :=
  renderOpt "cite" a.cite ++ renderOpt "datetime" a.datetime

/-- `<col>` -/
structure ColAttrs where
  span : Option String := none

def ColAttrs.render (a : ColAttrs) : String :=
  renderOpt "span" a.span

/-- `<fieldset>` -/
structure FieldsetAttrs where
  disabled : Bool := false
  name : Option String := none

def FieldsetAttrs.render (a : FieldsetAttrs) : String :=
  renderBoolAttr "disabled" a.disabled ++ renderOpt "name" a.name

/-- `<optgroup>` -/
structure OptgroupAttrs where
  label : String
  disabled : Bool := false

def OptgroupAttrs.render (a : OptgroupAttrs) : String :=
  renderAttr "label" a.label ++ renderBoolAttr "disabled" a.disabled

/-- `<output>`
`for_` (trailing underscore -- `for` is a Lean keyword). -/
structure OutputAttrs where
  for_ : Option String := none
  name : Option String := none

def OutputAttrs.render (a : OutputAttrs) : String :=
  renderOpt "for" a.for_ ++ renderOpt "name" a.name

/-- `<progress>` -/
structure ProgressAttrs where
  value : Option String := none
  max : Option String := none

def ProgressAttrs.render (a : ProgressAttrs) : String :=
  renderOpt "value" a.value ++ renderOpt "max" a.max

/-- `<meter>` -/
structure MeterAttrs where
  value : Option String := none
  min : Option String := none
  max : Option String := none
  low : Option String := none
  high : Option String := none
  optimum : Option String := none

def MeterAttrs.render (a : MeterAttrs) : String :=
  renderOpt "value" a.value ++ renderOpt "min" a.min ++ renderOpt "max" a.max ++
    renderOpt "low" a.low ++ renderOpt "high" a.high ++ renderOpt "optimum" a.optimum

/-- `<details>`/`<dialog>`
`open_` (trailing underscore -- `open` is a Lean keyword). -/
structure OpenAttrs where
  open_ : Bool := false

def OpenAttrs.render (a : OpenAttrs) : String :=
  renderBoolAttr "open" a.open_

/-- `<base>` -/
structure BaseAttrs where
  href : Option String := none
  target : Option String := none

def BaseAttrs.render (a : BaseAttrs) : String :=
  renderOpt "href" a.href ++ renderOpt "target" a.target

/-- `<canvas>` -/
structure CanvasAttrs where
  width : Option String := none
  height : Option String := none

def CanvasAttrs.render (a : CanvasAttrs) : String :=
  renderOpt "width" a.width ++ renderOpt "height" a.height

/-- `<slot>` -/
structure SlotAttrs where
  name : Option String := none

def SlotAttrs.render (a : SlotAttrs) : String :=
  renderOpt "name" a.name

/-- `<source>`
Dual-purpose in the HTML spec:
- inside `<picture>` it's `srcset`/`type`/`media` (no `src`)
- inside `<video>`/`<audio>` it's `src`/`type` (no `srcset`)
not distinguished here. -/
structure SourceAttrs where
  src : Option String := none
  srcset : Option String := none
  type : Option String := none
  media : Option String := none

def SourceAttrs.render (a : SourceAttrs) : String :=
  renderOpt "src" a.src ++ renderOpt "srcset" a.srcset ++ renderOpt "type" a.type ++
    renderOpt "media" a.media

/-- `<track>` -/
structure TrackAttrs where
  src : String
  kind : Option String := none
  srclang : Option String := none
  label : Option String := none
  default : Bool := false

def TrackAttrs.render (a : TrackAttrs) : String :=
  renderAttr "src" a.src ++ renderOpt "kind" a.kind ++ renderOpt "srclang" a.srclang ++
    renderOpt "label" a.label ++ renderBoolAttr "default" a.default

/-- `<iframe>` -/
structure IframeAttrs where
  src : String
  title : Option String := none
  width : Option String := none
  height : Option String := none

def IframeAttrs.render (a : IframeAttrs) : String :=
  renderAttr "src" a.src ++ renderOpt "title" a.title ++ renderOpt "width" a.width ++
    renderOpt "height" a.height

/-- `<embed>` -/
structure EmbedAttrs where
  src : Option String := none
  type : Option String := none
  width : Option String := none
  height : Option String := none

def EmbedAttrs.render (a : EmbedAttrs) : String :=
  renderOpt "src" a.src ++ renderOpt "type" a.type ++ renderOpt "width" a.width ++
    renderOpt "height" a.height

/-- `<object>` -/
structure ObjectAttrs where
  data : Option String := none
  type : Option String := none
  width : Option String := none
  height : Option String := none

def ObjectAttrs.render (a : ObjectAttrs) : String :=
  renderOpt "data" a.data ++ renderOpt "type" a.type ++ renderOpt "width" a.width ++
    renderOpt "height" a.height

/-- `<video>` -/
structure VideoAttrs where
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
    renderBoolAttr "muted" a.muted ++ renderOpt "width" a.width ++ renderOpt "height" a.height

/-- `<audio>` -/
structure AudioAttrs where
  src : Option String := none
  controls : Bool := false
  autoplay : Bool := false
  loop : Bool := false
  muted : Bool := false

def AudioAttrs.render (a : AudioAttrs) : String :=
  renderOpt "src" a.src ++ renderBoolAttr "controls" a.controls ++
    renderBoolAttr "autoplay" a.autoplay ++ renderBoolAttr "loop" a.loop ++
    renderBoolAttr "muted" a.muted

/-- `<map>` -/
structure MapAttrs where
  name : String

def MapAttrs.render (a : MapAttrs) : String :=
  renderAttr "name" a.name

/-- `<area>` -/
structure AreaAttrs where
  href : Option String := none
  alt : Option String := none
  shape : Option String := none
  coords : Option String := none
  target : Option String := none

def AreaAttrs.render (a : AreaAttrs) : String :=
  renderOpt "href" a.href ++ renderOpt "alt" a.alt ++ renderOpt "shape" a.shape ++
    renderOpt "coords" a.coords ++ renderOpt "target" a.target

end Html

import Html.Node
import Html.Escape
import Html.Attrs

/-!
Named tag functions, built on `Html/Node.lean`'s constructor shapes and
`Html/Attrs.lean`'s typed attribute vocabulary.

Scope notes:

- `html` is **not** defined here -- it's inseparable from the
  `<!DOCTYPE html>` prefix that makes it a document at all, so it stays
  `Html.document`'s sole responsibility rather than a general-purpose tag.
- Tags with well-known attributes beyond the global set get a dedicated
  `<Tag>Attrs` record in `Html/Attrs.lean` (`AAttrs`, `ImgAttrs`,
  `InputAttrs`, and ~35 others), which `extends HtmlAttrs` so a single
  `attrs` argument carries both. Tags with nothing beyond the global set
  -- `div`, `p`, `span`, `legend`, `summary`, `datalist`, and most other
  containers/text-level elements -- take plain `HtmlAttrs` directly, plus
  `rawAttrs`, which also covers any attribute not (yet) modeled as a
  typed field on a tag that does have a record. More typed records can be
  added later following the existing pattern.
- List (`ul`/`ol`/`menu`), table (`table`/`thead`/`tbody`/`tfoot`/`tr`),
  and `select`/`optgroup`/`datalist` children are constrained to their real
  HTML5 content models via dedicated categories in `Html/Node.lean`
  (`listItem`; `tableSection`/`tableRow`/`tableCell`/`tableColumn`;
  `option`/`selectChild`) rather than accepting general flow content. Order
  within a parent (e.g. HTML5 wants `<thead>` before `<tbody>`, `<caption>`
  first inside `<table>`) is still unchecked -- these categories only
  constrain *which* tags are valid children, not their sequence.
-/

namespace Html

private def combineAttrs (specific : String) (rawAttrs : List (String × String)) : String :=
  specific ++ renderRawAttrs rawAttrs

-- Structure: flow content, flow children.
def div (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "div" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def section_ (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "section" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def article (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "article" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def header (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "header" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def footer (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "footer" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def nav (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "nav" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def aside (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "aside" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def hgroup (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "hgroup" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def address (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "address" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def main (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "main" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def search (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "search" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def head (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "head" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def body (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "body" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def title (content : String) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.textElement .flow "title" content (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

/-- Takes `rawAttrs` as its primary content rather than a typed
attrs record, since a meta tag's shape varies by purpose --
`[("charset", "utf-8")]`, `[("name", "viewport"), ("content", "...")]`,
`[("http-equiv", "..."), ("content", "...")]`, ... -- with no one shape
common enough to single out as required fields. -/
def meta_ (rawAttrs : List (String × String)) (attrs : HtmlAttrs := {}) : Node .flow :=
  Node.voidElement .flow "meta" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def link (attrs : LinkAttrs) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "link" (combineAttrs (LinkAttrs.render attrs) rawAttrs)

def base (attrs : BaseAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "base" (combineAttrs (BaseAttrs.render attrs) rawAttrs)

def script (attrs : ScriptAttrs) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "script" [] (combineAttrs (ScriptAttrs.render attrs) rawAttrs)

def noscript (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "noscript" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def template (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "template" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def canvas (children : List (Node .flow)) (attrs : CanvasAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "canvas" children (combineAttrs (CanvasAttrs.render attrs) rawAttrs)

def slot (children : List (Node .phrasing)) (attrs : SlotAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "slot" children (combineAttrs (SlotAttrs.render attrs) rawAttrs)

def p (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "p" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h1 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "h1" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h2 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "h2" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h3 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "h3" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h4 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "h4" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h5 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "h5" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def h6 (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "h6" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ul (children : List (Node .listItem)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .listItem "ul" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ol (children : List (Node .listItem)) (attrs : OlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .listItem "ol" children (combineAttrs (OlAttrs.render attrs) rawAttrs)

def li (children : List (Node .flow)) (attrs : LiAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .listItem :=
  Node.elementOf .listItem .flow "li" children (combineAttrs (LiAttrs.render attrs) rawAttrs)

def menu (children : List (Node .listItem)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .listItem "menu" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dl (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "dl" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dt (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "dt" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dd (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "dd" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def blockquote (children : List (Node .flow)) (attrs : BlockquoteAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "blockquote" children (combineAttrs (BlockquoteAttrs.render attrs) rawAttrs)

def figure (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "figure" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def figcaption (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "figcaption" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

-- `pre`: flow content, phrasing-only children (preformatted text).
def pre (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "pre" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def code (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "code" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def a (attrs : AAttrs) (children : List (Node .phrasing))
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "a" children (combineAttrs (AAttrs.render attrs) rawAttrs)

def strong (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "strong" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def em (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "em" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def small (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "small" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def span (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "span" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def i (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "i" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def b (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "b" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def u (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "u" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def s (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "s" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def mark (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "mark" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def cite (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "cite" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dfn (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "dfn" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def abbr (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "abbr" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def var (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "var" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def samp (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "samp" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def kbd (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "kbd" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def sub (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "sub" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def sup (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "sup" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def bdi (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "bdi" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def bdo (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "bdo" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def ruby (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "ruby" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def rt (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "rt" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def rp (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "rp" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def wbr (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "wbr" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def q (children : List (Node .phrasing)) (attrs : QAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "q" children (combineAttrs (QAttrs.render attrs) rawAttrs)

def time (children : List (Node .phrasing)) (attrs : TimeAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "time" children (combineAttrs (TimeAttrs.render attrs) rawAttrs)

def data (attrs : DataAttrs) (children : List (Node .phrasing))
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "data" children (combineAttrs (DataAttrs.render attrs) rawAttrs)

/-- `ins`/`del` have HTML5's "transparent" content model: each takes on
whatever category its surrounding context allows, rather than having a
fixed category of its own. `cat` is left as a free, auto-bound implicit.
This makes `ins`/`del` usable directly inside a `<p>` (phrasing context)
or a `<div>` (flow context) alike, with no manual type ascription needed
at either call site. -/
def ins (children : List (Node cat)) (attrs : InsDelAttrs := {})
    (rawAttrs : List (String × String) := []) : Node cat :=
  Node.element cat "ins" children (combineAttrs (InsDelAttrs.render attrs) rawAttrs)

def del (children : List (Node cat)) (attrs : InsDelAttrs := {})
    (rawAttrs : List (String × String) := []) : Node cat :=
  Node.element cat "del" children (combineAttrs (InsDelAttrs.render attrs) rawAttrs)

def br (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "br" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def form (children : List (Node .flow)) (attrs : FormAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "form" children (combineAttrs (FormAttrs.render attrs) rawAttrs)

def fieldset (children : List (Node .flow)) (attrs : FieldsetAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "fieldset" children (combineAttrs (FieldsetAttrs.render attrs) rawAttrs)

def legend (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "legend" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def input (attrs : InputAttrs := {}) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "input" (combineAttrs (InputAttrs.render attrs) rawAttrs)

def label (children : List (Node .phrasing)) (attrs : LabelAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "label" children (combineAttrs (LabelAttrs.render attrs) rawAttrs)

def textarea (content : String) (attrs : TextareaAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.textElement .phrasing "textarea" content (combineAttrs (TextareaAttrs.render attrs) rawAttrs)

def option (label : String) (attrs : OptionAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .option :=
  Node.textElement .option "option" label (combineAttrs (OptionAttrs.render attrs) rawAttrs)

def select (children : List (Node .selectChild)) (attrs : SelectAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.elementOf .phrasing .selectChild "select" children
    (combineAttrs (SelectAttrs.render attrs) rawAttrs)

def datalist (children : List (Node .option)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.elementOf .phrasing .option "datalist" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def optgroup (attrs : OptgroupAttrs) (children : List (Node .option))
    (rawAttrs : List (String × String) := []) : Node .selectChild :=
  Node.elementOf .selectChild .option "optgroup" children
    (combineAttrs (OptgroupAttrs.render attrs) rawAttrs)

def button (children : List (Node .phrasing)) (attrs : ButtonAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "button" children (combineAttrs (ButtonAttrs.render attrs) rawAttrs)

def output (children : List (Node .phrasing)) (attrs : OutputAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "output" children (combineAttrs (OutputAttrs.render attrs) rawAttrs)

def progress (children : List (Node .phrasing)) (attrs : ProgressAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "progress" children (combineAttrs (ProgressAttrs.render attrs) rawAttrs)

def meter (children : List (Node .phrasing)) (attrs : MeterAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.element .phrasing "meter" children (combineAttrs (MeterAttrs.render attrs) rawAttrs)

def details (children : List (Node .flow)) (attrs : OpenAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "details" children (combineAttrs (OpenAttrs.render attrs) rawAttrs)

def summary (children : List (Node .phrasing)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .phrasing "summary" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def dialog (children : List (Node .flow)) (attrs : OpenAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "dialog" children (combineAttrs (OpenAttrs.render attrs) rawAttrs)

def img (attrs : ImgAttrs) (rawAttrs : List (String × String) := []) : Node .phrasing :=
  Node.voidElement .phrasing "img" (combineAttrs (ImgAttrs.render attrs) rawAttrs)

def hr (attrs : HtmlAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "hr" (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def picture (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "picture" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def source (attrs : SourceAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "source" (combineAttrs (SourceAttrs.render attrs) rawAttrs)

def track (attrs : TrackAttrs) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "track" (combineAttrs (TrackAttrs.render attrs) rawAttrs)

def iframe (attrs : IframeAttrs) (children : List (Node .flow) := [])
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "iframe" children (combineAttrs (IframeAttrs.render attrs) rawAttrs)

def embed (attrs : EmbedAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "embed" (combineAttrs (EmbedAttrs.render attrs) rawAttrs)

def object (attrs : ObjectAttrs := {}) (children : List (Node .flow) := [])
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "object" children (combineAttrs (ObjectAttrs.render attrs) rawAttrs)

def video (children : List (Node .flow) := []) (attrs : VideoAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "video" children (combineAttrs (VideoAttrs.render attrs) rawAttrs)

def audio (children : List (Node .flow) := []) (attrs : AudioAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "audio" children (combineAttrs (AudioAttrs.render attrs) rawAttrs)

def map (attrs : MapAttrs) (children : List (Node .flow) := [])
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.element .flow "map" children (combineAttrs (MapAttrs.render attrs) rawAttrs)

def area (attrs : AreaAttrs := {}) (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.voidElement .flow "area" (combineAttrs (AreaAttrs.render attrs) rawAttrs)

def table (children : List (Node .tableSection)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .flow :=
  Node.elementOf .flow .tableSection "table" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def caption (children : List (Node .flow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableSection :=
  Node.elementOf .tableSection .flow "caption" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def colgroup (children : List (Node .tableColumn)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableSection :=
  Node.elementOf .tableSection .tableColumn "colgroup" children
    (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def col (attrs : ColAttrs := {}) (rawAttrs : List (String × String) := []) : Node .tableColumn :=
  Node.voidElement .tableColumn "col" (combineAttrs (ColAttrs.render attrs) rawAttrs)

def thead (children : List (Node .tableRow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableSection :=
  Node.elementOf .tableSection .tableRow "thead" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tbody (children : List (Node .tableRow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableSection :=
  Node.elementOf .tableSection .tableRow "tbody" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tfoot (children : List (Node .tableRow)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableSection :=
  Node.elementOf .tableSection .tableRow "tfoot" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def tr (children : List (Node .tableCell)) (attrs : HtmlAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableRow :=
  Node.elementOf .tableRow .tableCell "tr" children (combineAttrs (HtmlAttrs.render attrs) rawAttrs)

def th (children : List (Node .flow)) (attrs : ThAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCell :=
  Node.elementOf .tableCell .flow "th" children (combineAttrs (ThAttrs.render attrs) rawAttrs)

def td (children : List (Node .flow)) (attrs : TdAttrs := {})
    (rawAttrs : List (String × String) := []) : Node .tableCell :=
  Node.elementOf .tableCell .flow "td" children (combineAttrs (TdAttrs.render attrs) rawAttrs)

end Html

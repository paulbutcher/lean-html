import Html.Attrs

namespace HtmlTests

open Html

#guard (HtmlAttrs.render {}).render = ""
#guard (HtmlAttrs.render { id := "x" }).render = " id=\"x\""
#guard (HtmlAttrs.render { class_ := "a b" }).render = " class=\"a b\""
#guard (HtmlAttrs.render { style := "color:red" }).render = " style=\"color:red\""
#guard (HtmlAttrs.render { title := "t" }).render = " title=\"t\""
#guard (HtmlAttrs.render { lang := "en" }).render = " lang=\"en\""
#guard (HtmlAttrs.render { dir := "ltr" }).render = " dir=\"ltr\""
#guard (HtmlAttrs.render { id := "x", class_ := "y" }).render = " id=\"x\" class=\"y\""
#guard (HtmlAttrs.render { id := "x\"y" }).render = " id=\"x&quot;y\""  -- values still escaped

/--
error: Type mismatch
  true
has type
  Bool
but is expected to have type
  Option String
-/
#guard_msgs in
example := HtmlAttrs.render { id := true }

#guard (AAttrs.render { href := "https://example.com" }).render = " href=\"https://example.com\""
#guard (AAttrs.render { href := "x", target := "_blank" }).render = " href=\"x\" target=\"_blank\""

#guard (ImgAttrs.render { src := "a.png", alt := "desc" }).render = " src=\"a.png\" alt=\"desc\""

#guard (ScriptAttrs.render { src := "/a.js" }).render = " src=\"/a.js\""
#guard (ScriptAttrs.render { src := "/a.js", integrity := "sha384-x", crossorigin := "anonymous" }).render
  = " src=\"/a.js\" integrity=\"sha384-x\" crossorigin=\"anonymous\""

#guard (LinkAttrs.render { rel := "stylesheet", href := "/style.css" }).render
  = " rel=\"stylesheet\" href=\"/style.css\""

#guard (QAttrs.render {}).render = ""
#guard (QAttrs.render { cite := "https://example.com" }).render = " cite=\"https://example.com\""

#guard (TimeAttrs.render {}).render = ""
#guard (TimeAttrs.render { datetime := "2026-07-14" }).render = " datetime=\"2026-07-14\""

#guard (DataAttrs.render { value := "42" }).render = " value=\"42\""

#guard (InsDelAttrs.render {}).render = ""
#guard (InsDelAttrs.render { cite := "https://example.com", datetime := "2026-07-14" }).render
  = " cite=\"https://example.com\" datetime=\"2026-07-14\""

#guard (ColAttrs.render {}).render = ""
#guard (ColAttrs.render { span := "2" }).render = " span=\"2\""

#guard (FieldsetAttrs.render {}).render = ""
#guard (FieldsetAttrs.render { disabled := true, name := "x" }).render = " disabled name=\"x\""

#guard (OptgroupAttrs.render { label := "Fruit" }).render = " label=\"Fruit\""
#guard (OptgroupAttrs.render { label := "Fruit", disabled := true }).render = " label=\"Fruit\" disabled"

#guard (OutputAttrs.render {}).render = ""
#guard (OutputAttrs.render { for_ := "a b", name := "result" }).render = " for=\"a b\" name=\"result\""

#guard (ProgressAttrs.render {}).render = ""
#guard (ProgressAttrs.render { value := "50", max := "100" }).render = " value=\"50\" max=\"100\""

#guard (MeterAttrs.render {}).render = ""
#guard (MeterAttrs.render { value := "6", min := "0", max := "10" }).render
  = " value=\"6\" min=\"0\" max=\"10\""

#guard (OpenAttrs.render {}).render = ""
#guard (OpenAttrs.render { open_ := true }).render = " open"

#guard (BaseAttrs.render {}).render = ""
#guard (BaseAttrs.render { href := "/", target := "_blank" }).render = " href=\"/\" target=\"_blank\""

#guard (CanvasAttrs.render {}).render = ""
#guard (CanvasAttrs.render { width := "300", height := "150" }).render = " width=\"300\" height=\"150\""

#guard (SlotAttrs.render {}).render = ""
#guard (SlotAttrs.render { name := "header" }).render = " name=\"header\""

#guard (SourceAttrs.render {}).render = ""
#guard (SourceAttrs.render { src := "a.mp4", type := "video/mp4" }).render
  = " src=\"a.mp4\" type=\"video/mp4\""

#guard (TrackAttrs.render { src := "a.vtt" }).render = " src=\"a.vtt\""
#guard (TrackAttrs.render { src := "a.vtt", kind := "subtitles", srclang := "en", default := true }).render
  = " src=\"a.vtt\" kind=\"subtitles\" srclang=\"en\" default"

#guard (IframeAttrs.render { src := "/embed" }).render = " src=\"/embed\""
#guard (IframeAttrs.render { src := "/embed", width := "300", height := "150", title := "t" }).render
  = " src=\"/embed\" width=\"300\" height=\"150\" title=\"t\""

#guard (EmbedAttrs.render {}).render = ""
#guard (EmbedAttrs.render { src := "a.swf", type := "application/x-shockwave-flash" }).render
  = " src=\"a.swf\" type=\"application/x-shockwave-flash\""

#guard (ObjectAttrs.render {}).render = ""
#guard (ObjectAttrs.render { data := "a.pdf", type := "application/pdf" }).render
  = " data=\"a.pdf\" type=\"application/pdf\""

#guard (VideoAttrs.render {}).render = ""
#guard (VideoAttrs.render { src := "a.mp4", controls := true }).render = " src=\"a.mp4\" controls"

#guard (AudioAttrs.render {}).render = ""
#guard (AudioAttrs.render { src := "a.mp3", controls := true }).render = " src=\"a.mp3\" controls"

#guard (MapAttrs.render { name := "sitemap" }).render = " name=\"sitemap\""

#guard (AreaAttrs.render {}).render = ""
#guard (AreaAttrs.render { href := "#a", alt := "Area A", shape := "rect", coords := "0,0,10,10" }).render
  = " href=\"#a\" alt=\"Area A\" shape=\"rect\" coords=\"0,0,10,10\""

#guard (InputAttrs.render {}).render = " type=\"text\""
#guard (InputAttrs.render { disabled := true }).render = " type=\"text\" disabled"
#guard (InputAttrs.render { disabled := false }).render = " type=\"text\""  -- explicit: never `disabled="false"`
#guard (InputAttrs.render { checked := true, required := true }).render = " type=\"text\" checked required"
#guard (InputAttrs.render { name := "q", value := "v" }).render = " type=\"text\" name=\"q\" value=\"v\""

#guard (OlAttrs.render {}).render = ""
#guard (OlAttrs.render { reversed := true }).render = " reversed"
#guard (OlAttrs.render { start := "5" }).render = " start=\"5\""
#guard (OlAttrs.render { type := "a" }).render = " type=\"a\""
#guard (OlAttrs.render { reversed := true, start := "5", type := "a" }).render
  = " reversed start=\"5\" type=\"a\""

#guard (LiAttrs.render {}).render = ""
#guard (LiAttrs.render { value := "3" }).render = " value=\"3\""

#guard (BlockquoteAttrs.render {}).render = ""
#guard (BlockquoteAttrs.render { cite := "https://example.com" }).render = " cite=\"https://example.com\""

#guard (FormAttrs.render {}).render = ""
#guard (FormAttrs.render { action := "/submit" }).render = " action=\"/submit\""
#guard (FormAttrs.render { method := "post" }).render = " method=\"post\""
#guard (FormAttrs.render { action := "/submit", method := "post" }).render
  = " action=\"/submit\" method=\"post\""

#guard (LabelAttrs.render {}).render = ""
#guard (LabelAttrs.render { for_ := "name" }).render = " for=\"name\""

#guard (TextareaAttrs.render {}).render = ""
#guard (TextareaAttrs.render { name := "bio" }).render = " name=\"bio\""
#guard (TextareaAttrs.render { placeholder := "Tell us more" }).render = " placeholder=\"Tell us more\""
#guard (TextareaAttrs.render { rows := "4" }).render = " rows=\"4\""
#guard (TextareaAttrs.render { cols := "50" }).render = " cols=\"50\""
#guard (TextareaAttrs.render { disabled := true }).render = " disabled"
#guard (TextareaAttrs.render { readonly := true }).render = " readonly"
#guard (TextareaAttrs.render { required := true }).render = " required"
#guard (TextareaAttrs.render { autofocus := true }).render = " autofocus"
#guard (TextareaAttrs.render { name := "bio", placeholder := "Tell us more", rows := "4", cols := "50", disabled := true, readonly := true, required := true, autofocus := true }).render
  = " name=\"bio\" placeholder=\"Tell us more\" rows=\"4\" cols=\"50\" disabled readonly required autofocus"

#guard (OptionAttrs.render {}).render = ""
#guard (OptionAttrs.render { value := "1" }).render = " value=\"1\""
#guard (OptionAttrs.render { selected := true }).render = " selected"
#guard (OptionAttrs.render { disabled := true }).render = " disabled"
#guard (OptionAttrs.render { label := "One" }).render = " label=\"One\""
#guard (OptionAttrs.render { value := "1", selected := true, disabled := true, label := "One" }).render
  = " value=\"1\" selected disabled label=\"One\""

#guard (SelectAttrs.render {}).render = ""
#guard (SelectAttrs.render { name := "color" }).render = " name=\"color\""
#guard (SelectAttrs.render { multiple := true }).render = " multiple"
#guard (SelectAttrs.render { disabled := true }).render = " disabled"
#guard (SelectAttrs.render { required := true }).render = " required"
#guard (SelectAttrs.render { size := "4" }).render = " size=\"4\""
#guard (SelectAttrs.render { name := "color", multiple := true, disabled := true, required := true, size := "4" }).render
  = " name=\"color\" multiple disabled required size=\"4\""

#guard (ButtonAttrs.render {}).render = " type=\"submit\""
#guard (ButtonAttrs.render { type := "reset" }).render = " type=\"reset\""
#guard (ButtonAttrs.render { name := "action" }).render = " type=\"submit\" name=\"action\""
#guard (ButtonAttrs.render { value := "go" }).render = " type=\"submit\" value=\"go\""
#guard (ButtonAttrs.render { disabled := true }).render = " type=\"submit\" disabled"
#guard (ButtonAttrs.render { type := "reset", name := "action", value := "go", disabled := true }).render
  = " type=\"reset\" name=\"action\" value=\"go\" disabled"

#guard (ThAttrs.render {}).render = ""
#guard (ThAttrs.render { colspan := "2" }).render = " colspan=\"2\""
#guard (ThAttrs.render { rowspan := "3" }).render = " rowspan=\"3\""
#guard (ThAttrs.render { scope := "col" }).render = " scope=\"col\""
#guard (ThAttrs.render { colspan := "2", rowspan := "3", scope := "col" }).render
  = " colspan=\"2\" rowspan=\"3\" scope=\"col\""

#guard (TdAttrs.render {}).render = ""
#guard (TdAttrs.render { colspan := "2" }).render = " colspan=\"2\""
#guard (TdAttrs.render { rowspan := "3" }).render = " rowspan=\"3\""
#guard (TdAttrs.render { colspan := "2", rowspan := "3" }).render = " colspan=\"2\" rowspan=\"3\""

#guard renderBoolAttr "disabled" true = " disabled"
#guard renderBoolAttr "disabled" false = ""

#guard renderRawAttrs [("data-x", "a\"b")] = " data-x=\"a&quot;b\""
#guard renderRawAttrs [("hx-get", "/x"), ("hx-target", "#y")] = " hx-get=\"/x\" hx-target=\"#y\""
#guard renderRawAttrs [("evil onmouseover=\"alert(1)", "x")]
  = " evil_onmouseover__alert_1_=\"x\""  -- disallowed chars in the name are sanitized, not passed through
#guard renderRawAttrs [("   ", "x")] = " ___=\"x\""  -- each disallowed char becomes its own "_"
#guard renderRawAttrs [("", "x")] = " _=\"x\""  -- an empty name sanitizes to nothing, falls back to "_"

end HtmlTests

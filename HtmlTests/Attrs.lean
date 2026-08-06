import Html.Attrs

namespace HtmlTests

open Html

#guard HtmlAttrs.render {} = ""
#guard HtmlAttrs.render { id := "x" } = " id=\"x\""
#guard HtmlAttrs.render { class_ := "a b" } = " class=\"a b\""
#guard HtmlAttrs.render { style := "color:red" } = " style=\"color:red\""
#guard HtmlAttrs.render { title := "t" } = " title=\"t\""
#guard HtmlAttrs.render { lang := "en" } = " lang=\"en\""
#guard HtmlAttrs.render { dir := "ltr" } = " dir=\"ltr\""
#guard HtmlAttrs.render { id := "x", class_ := "y" } = " id=\"x\" class=\"y\""
#guard HtmlAttrs.render { id := "x\"y" } = " id=\"x&quot;y\""  -- values still escaped

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

#guard AAttrs.render { href := "https://example.com" } = " href=\"https://example.com\""
#guard AAttrs.render { href := "x", target := "_blank" } = " href=\"x\" target=\"_blank\""

#guard ImgAttrs.render { src := "a.png", alt := "desc" } = " src=\"a.png\" alt=\"desc\""

#guard ScriptAttrs.render { src := "/a.js" } = " src=\"/a.js\""
#guard ScriptAttrs.render { src := "/a.js", integrity := "sha384-x", crossorigin := "anonymous" }
  = " src=\"/a.js\" integrity=\"sha384-x\" crossorigin=\"anonymous\""

#guard LinkAttrs.render { rel := "stylesheet", href := "/style.css" }
  = " rel=\"stylesheet\" href=\"/style.css\""

#guard QAttrs.render {} = ""
#guard QAttrs.render { cite := "https://example.com" } = " cite=\"https://example.com\""

#guard TimeAttrs.render {} = ""
#guard TimeAttrs.render { datetime := "2026-07-14" } = " datetime=\"2026-07-14\""

#guard DataAttrs.render { value := "42" } = " value=\"42\""

#guard InsDelAttrs.render {} = ""
#guard InsDelAttrs.render { cite := "https://example.com", datetime := "2026-07-14" }
  = " cite=\"https://example.com\" datetime=\"2026-07-14\""

#guard ColAttrs.render {} = ""
#guard ColAttrs.render { span := "2" } = " span=\"2\""

#guard FieldsetAttrs.render {} = ""
#guard FieldsetAttrs.render { disabled := true, name := "x" } = " disabled name=\"x\""

#guard OptgroupAttrs.render { label := "Fruit" } = " label=\"Fruit\""
#guard OptgroupAttrs.render { label := "Fruit", disabled := true } = " label=\"Fruit\" disabled"

#guard OutputAttrs.render {} = ""
#guard OutputAttrs.render { for_ := "a b", name := "result" } = " for=\"a b\" name=\"result\""

#guard ProgressAttrs.render {} = ""
#guard ProgressAttrs.render { value := "50", max := "100" } = " value=\"50\" max=\"100\""

#guard MeterAttrs.render {} = ""
#guard MeterAttrs.render { value := "6", min := "0", max := "10" }
  = " value=\"6\" min=\"0\" max=\"10\""

#guard OpenAttrs.render {} = ""
#guard OpenAttrs.render { open_ := true } = " open"

#guard BaseAttrs.render {} = ""
#guard BaseAttrs.render { href := "/", target := "_blank" } = " href=\"/\" target=\"_blank\""

#guard CanvasAttrs.render {} = ""
#guard CanvasAttrs.render { width := "300", height := "150" } = " width=\"300\" height=\"150\""

#guard SlotAttrs.render {} = ""
#guard SlotAttrs.render { name := "header" } = " name=\"header\""

#guard SourceAttrs.render {} = ""
#guard SourceAttrs.render { src := "a.mp4", type := "video/mp4" }
  = " src=\"a.mp4\" type=\"video/mp4\""

#guard TrackAttrs.render { src := "a.vtt" } = " src=\"a.vtt\""
#guard TrackAttrs.render { src := "a.vtt", kind := "subtitles", srclang := "en", default := true }
  = " src=\"a.vtt\" kind=\"subtitles\" srclang=\"en\" default"

#guard IframeAttrs.render { src := "/embed" } = " src=\"/embed\""
#guard IframeAttrs.render { src := "/embed", width := "300", height := "150", title := "t" }
  = " src=\"/embed\" width=\"300\" height=\"150\" title=\"t\""

#guard EmbedAttrs.render {} = ""
#guard EmbedAttrs.render { src := "a.swf", type := "application/x-shockwave-flash" }
  = " src=\"a.swf\" type=\"application/x-shockwave-flash\""

#guard ObjectAttrs.render {} = ""
#guard ObjectAttrs.render { data := "a.pdf", type := "application/pdf" }
  = " data=\"a.pdf\" type=\"application/pdf\""

#guard VideoAttrs.render {} = ""
#guard VideoAttrs.render { src := "a.mp4", controls := true } = " src=\"a.mp4\" controls"

#guard AudioAttrs.render {} = ""
#guard AudioAttrs.render { src := "a.mp3", controls := true } = " src=\"a.mp3\" controls"

#guard MapAttrs.render { name := "sitemap" } = " name=\"sitemap\""

#guard AreaAttrs.render {} = ""
#guard AreaAttrs.render { href := "#a", alt := "Area A", shape := "rect", coords := "0,0,10,10" }
  = " href=\"#a\" alt=\"Area A\" shape=\"rect\" coords=\"0,0,10,10\""

#guard InputAttrs.render {} = " type=\"text\""
#guard InputAttrs.render { disabled := true } = " type=\"text\" disabled"
#guard InputAttrs.render { disabled := false } = " type=\"text\""  -- explicit: never `disabled="false"`
#guard InputAttrs.render { checked := true, required := true } = " type=\"text\" checked required"
#guard InputAttrs.render { name := "q", value := "v" } = " type=\"text\" name=\"q\" value=\"v\""

#guard OlAttrs.render {} = ""
#guard OlAttrs.render { reversed := true } = " reversed"
#guard OlAttrs.render { start := "5" } = " start=\"5\""
#guard OlAttrs.render { type := "a" } = " type=\"a\""
#guard OlAttrs.render { reversed := true, start := "5", type := "a" }
  = " reversed start=\"5\" type=\"a\""

#guard LiAttrs.render {} = ""
#guard LiAttrs.render { value := "3" } = " value=\"3\""

#guard BlockquoteAttrs.render {} = ""
#guard BlockquoteAttrs.render { cite := "https://example.com" } = " cite=\"https://example.com\""

#guard FormAttrs.render {} = ""
#guard FormAttrs.render { action := "/submit" } = " action=\"/submit\""
#guard FormAttrs.render { method := "post" } = " method=\"post\""
#guard FormAttrs.render { action := "/submit", method := "post" }
  = " action=\"/submit\" method=\"post\""

#guard LabelAttrs.render {} = ""
#guard LabelAttrs.render { for_ := "name" } = " for=\"name\""

#guard TextareaAttrs.render {} = ""
#guard TextareaAttrs.render { name := "bio" } = " name=\"bio\""
#guard TextareaAttrs.render { placeholder := "Tell us more" } = " placeholder=\"Tell us more\""
#guard TextareaAttrs.render { rows := "4" } = " rows=\"4\""
#guard TextareaAttrs.render { cols := "50" } = " cols=\"50\""
#guard TextareaAttrs.render { disabled := true } = " disabled"
#guard TextareaAttrs.render { readonly := true } = " readonly"
#guard TextareaAttrs.render { required := true } = " required"
#guard TextareaAttrs.render { autofocus := true } = " autofocus"
#guard TextareaAttrs.render { name := "bio", placeholder := "Tell us more", rows := "4", cols := "50", disabled := true, readonly := true, required := true, autofocus := true }
  = " name=\"bio\" placeholder=\"Tell us more\" rows=\"4\" cols=\"50\" disabled readonly required autofocus"

#guard OptionAttrs.render {} = ""
#guard OptionAttrs.render { value := "1" } = " value=\"1\""
#guard OptionAttrs.render { selected := true } = " selected"
#guard OptionAttrs.render { disabled := true } = " disabled"
#guard OptionAttrs.render { label := "One" } = " label=\"One\""
#guard OptionAttrs.render { value := "1", selected := true, disabled := true, label := "One" }
  = " value=\"1\" selected disabled label=\"One\""

#guard SelectAttrs.render {} = ""
#guard SelectAttrs.render { name := "color" } = " name=\"color\""
#guard SelectAttrs.render { multiple := true } = " multiple"
#guard SelectAttrs.render { disabled := true } = " disabled"
#guard SelectAttrs.render { required := true } = " required"
#guard SelectAttrs.render { size := "4" } = " size=\"4\""
#guard SelectAttrs.render { name := "color", multiple := true, disabled := true, required := true, size := "4" }
  = " name=\"color\" multiple disabled required size=\"4\""

#guard ButtonAttrs.render {} = " type=\"submit\""
#guard ButtonAttrs.render { type := "reset" } = " type=\"reset\""
#guard ButtonAttrs.render { name := "action" } = " type=\"submit\" name=\"action\""
#guard ButtonAttrs.render { value := "go" } = " type=\"submit\" value=\"go\""
#guard ButtonAttrs.render { disabled := true } = " type=\"submit\" disabled"
#guard ButtonAttrs.render { type := "reset", name := "action", value := "go", disabled := true }
  = " type=\"reset\" name=\"action\" value=\"go\" disabled"

#guard ThAttrs.render {} = ""
#guard ThAttrs.render { colspan := "2" } = " colspan=\"2\""
#guard ThAttrs.render { rowspan := "3" } = " rowspan=\"3\""
#guard ThAttrs.render { scope := "col" } = " scope=\"col\""
#guard ThAttrs.render { colspan := "2", rowspan := "3", scope := "col" }
  = " colspan=\"2\" rowspan=\"3\" scope=\"col\""

#guard TdAttrs.render {} = ""
#guard TdAttrs.render { colspan := "2" } = " colspan=\"2\""
#guard TdAttrs.render { rowspan := "3" } = " rowspan=\"3\""
#guard TdAttrs.render { colspan := "2", rowspan := "3" } = " colspan=\"2\" rowspan=\"3\""

#guard renderBoolAttr "disabled" true = " disabled"
#guard renderBoolAttr "disabled" false = ""

#guard renderRawAttrs [("data-x", "a\"b")] = " data-x=\"a&quot;b\""
#guard renderRawAttrs [("hx-get", "/x"), ("hx-target", "#y")] = " hx-get=\"/x\" hx-target=\"#y\""
#guard renderRawAttrs [("evil onmouseover=\"alert(1)", "x")]
  = " evil_onmouseover__alert_1_=\"x\""  -- disallowed chars in the name are sanitized, not passed through
#guard renderRawAttrs [("   ", "x")] = " ___=\"x\""  -- each disallowed char becomes its own "_"
#guard renderRawAttrs [("", "x")] = " _=\"x\""  -- an empty name sanitizes to nothing, falls back to "_"

end HtmlTests

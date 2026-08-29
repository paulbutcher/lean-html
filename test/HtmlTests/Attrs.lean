/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

import Html.Attrs
meta import Html.Attrs

namespace HtmlTests

open Html

-- One guard per attribute record, with every field populated. What a guard
-- pins that `HtmlTests/Escape.lean`'s theorems can't is the part of a record
-- that is literal text rather than structure: each attribute's spelling, its
-- position in the emission order, and the inherited `HtmlAttrs` tail (`id`
-- here) coming last. The semantics of the primitives those records are built
-- from, that `++` concatenates renderings, that an absent `Option` and a
-- `false` flag contribute nothing, are proved there once for all names.

#guard (HtmlAttrs.render {}).render = ""
#guard (HtmlAttrs.render
    { id := "i", class_ := "c", style := "s", title := "t", lang := "en", dir := "ltr" }).render
  = " id=\"i\" class=\"c\" style=\"s\" title=\"t\" lang=\"en\" dir=\"ltr\""

#guard (AAttrs.render { href := "h", target := "_blank", rel := "noopener", id := "i" }).render
  = " href=\"h\" target=\"_blank\" rel=\"noopener\" id=\"i\""

#guard (ImgAttrs.render { src := "a.png", alt := "desc", id := "i" }).render
  = " src=\"a.png\" alt=\"desc\" id=\"i\""

-- `type` defaults to `text` rather than being absent.
#guard (InputAttrs.render {}).render = " type=\"text\""
#guard (InputAttrs.render
    { type := "checkbox", name := "n", value := "v", placeholder := "p", disabled := true,
      checked := true, required := true, readonly := true, id := "i" }).render
  = " type=\"checkbox\" name=\"n\" value=\"v\" placeholder=\"p\" disabled checked required readonly id=\"i\""

#guard (ScriptAttrs.render
    { src := "/a.js", integrity := "sha384-x", crossorigin := "anonymous", id := "i" }).render
  = " src=\"/a.js\" integrity=\"sha384-x\" crossorigin=\"anonymous\" id=\"i\""

#guard (LinkAttrs.render { rel := "stylesheet", href := "/style.css", id := "i" }).render
  = " rel=\"stylesheet\" href=\"/style.css\" id=\"i\""

#guard (QAttrs.render { cite := "https://example.com", id := "i" }).render
  = " cite=\"https://example.com\" id=\"i\""

#guard (TimeAttrs.render { datetime := "2026-07-14", id := "i" }).render
  = " datetime=\"2026-07-14\" id=\"i\""

#guard (DataAttrs.render { value := "42", id := "i" }).render = " value=\"42\" id=\"i\""

#guard (InsDelAttrs.render
    { cite := "https://example.com", datetime := "2026-07-14", id := "i" }).render
  = " cite=\"https://example.com\" datetime=\"2026-07-14\" id=\"i\""

#guard (ColAttrs.render { span := "2", id := "i" }).render = " span=\"2\" id=\"i\""

#guard (FieldsetAttrs.render { disabled := true, name := "n", id := "i" }).render
  = " disabled name=\"n\" id=\"i\""

#guard (OptgroupAttrs.render { label := "Fruit", disabled := true, id := "i" }).render
  = " label=\"Fruit\" disabled id=\"i\""

#guard (OutputAttrs.render { for_ := "a b", name := "result", id := "i" }).render
  = " for=\"a b\" name=\"result\" id=\"i\""

#guard (ProgressAttrs.render { value := "50", max := "100", id := "i" }).render
  = " value=\"50\" max=\"100\" id=\"i\""

#guard (MeterAttrs.render
    { value := "6", min := "0", max := "10", low := "2", high := "8", optimum := "7",
      id := "i" }).render
  = " value=\"6\" min=\"0\" max=\"10\" low=\"2\" high=\"8\" optimum=\"7\" id=\"i\""

#guard (OpenAttrs.render { open_ := true, id := "i" }).render = " open id=\"i\""

#guard (BaseAttrs.render { href := "/", target := "_blank", id := "i" }).render
  = " href=\"/\" target=\"_blank\" id=\"i\""

#guard (CanvasAttrs.render { width := "300", height := "150", id := "i" }).render
  = " width=\"300\" height=\"150\" id=\"i\""

#guard (SlotAttrs.render { name := "header", id := "i" }).render = " name=\"header\" id=\"i\""

#guard (SourceAttrs.render
    { src := "a.mp4", srcset := "a-2x.mp4 2x", type := "video/mp4",
      media := "(min-width: 600px)", id := "i" }).render
  = " src=\"a.mp4\" srcset=\"a-2x.mp4 2x\" type=\"video/mp4\" media=\"(min-width: 600px)\" id=\"i\""

#guard (TrackAttrs.render
    { src := "a.vtt", kind := "subtitles", srclang := "en", label := "English",
      default := true, id := "i" }).render
  = " src=\"a.vtt\" kind=\"subtitles\" srclang=\"en\" label=\"English\" default id=\"i\""

-- `title` is inherited from `HtmlAttrs`, so it renders in the global tail
-- rather than alongside `<iframe>`'s own attributes.
#guard (IframeAttrs.render
    { src := "/embed", width := "300", height := "150", title := "t", id := "i" }).render
  = " src=\"/embed\" width=\"300\" height=\"150\" id=\"i\" title=\"t\""

#guard (EmbedAttrs.render
    { src := "a.swf", type := "application/x-shockwave-flash", width := "300",
      height := "150", id := "i" }).render
  = " src=\"a.swf\" type=\"application/x-shockwave-flash\" width=\"300\" height=\"150\" id=\"i\""

#guard (ObjectAttrs.render
    { data := "a.pdf", type := "application/pdf", width := "300", height := "150",
      id := "i" }).render
  = " data=\"a.pdf\" type=\"application/pdf\" width=\"300\" height=\"150\" id=\"i\""

#guard (VideoAttrs.render
    { src := "a.mp4", poster := "p.png", controls := true, autoplay := true, loop := true,
      muted := true, width := "300", height := "150", id := "i" }).render
  = " src=\"a.mp4\" poster=\"p.png\" controls autoplay loop muted width=\"300\" height=\"150\" id=\"i\""

#guard (AudioAttrs.render
    { src := "a.mp3", controls := true, autoplay := true, loop := true, muted := true,
      id := "i" }).render
  = " src=\"a.mp3\" controls autoplay loop muted id=\"i\""

#guard (MapAttrs.render { name := "sitemap", id := "i" }).render = " name=\"sitemap\" id=\"i\""

#guard (AreaAttrs.render
    { href := "#a", alt := "Area A", shape := "rect", coords := "0,0,10,10",
      target := "_blank", id := "i" }).render
  = " href=\"#a\" alt=\"Area A\" shape=\"rect\" coords=\"0,0,10,10\" target=\"_blank\" id=\"i\""

#guard (OlAttrs.render { reversed := true, start := "5", type := "a", id := "i" }).render
  = " reversed start=\"5\" type=\"a\" id=\"i\""

#guard (LiAttrs.render { value := "3", id := "i" }).render = " value=\"3\" id=\"i\""

#guard (BlockquoteAttrs.render { cite := "https://example.com", id := "i" }).render
  = " cite=\"https://example.com\" id=\"i\""

#guard (FormAttrs.render { action := "/submit", method := "post", id := "i" }).render
  = " action=\"/submit\" method=\"post\" id=\"i\""

#guard (LabelAttrs.render { for_ := "name", id := "i" }).render = " for=\"name\" id=\"i\""

#guard (TextareaAttrs.render
    { name := "bio", placeholder := "Tell us more", rows := "4", cols := "50",
      disabled := true, readonly := true, required := true, autofocus := true,
      id := "i" }).render
  = " name=\"bio\" placeholder=\"Tell us more\" rows=\"4\" cols=\"50\" disabled readonly required autofocus id=\"i\""

#guard (OptionAttrs.render
    { value := "1", selected := true, disabled := true, label := "One", id := "i" }).render
  = " value=\"1\" selected disabled label=\"One\" id=\"i\""

#guard (SelectAttrs.render
    { name := "color", multiple := true, disabled := true, required := true, size := "4",
      id := "i" }).render
  = " name=\"color\" multiple disabled required size=\"4\" id=\"i\""

-- `type` defaults to `submit` rather than being absent.
#guard (ButtonAttrs.render {}).render = " type=\"submit\""
#guard (ButtonAttrs.render
    { type := "reset", name := "action", value := "go", disabled := true, id := "i" }).render
  = " type=\"reset\" name=\"action\" value=\"go\" disabled id=\"i\""

#guard (ThAttrs.render { colspan := "2", rowspan := "3", scope := "col", id := "i" }).render
  = " colspan=\"2\" rowspan=\"3\" scope=\"col\" id=\"i\""

#guard (TdAttrs.render { colspan := "2", rowspan := "3", id := "i" }).render
  = " colspan=\"2\" rowspan=\"3\" id=\"i\""

#guard renderRawAttrs [("data-x", "a\"b")] = " data-x=\"a&quot;b\""
#guard renderRawAttrs [("hx-get", "/x"), ("hx-target", "#y")] = " hx-get=\"/x\" hx-target=\"#y\""
#guard renderRawAttrs [("evil onmouseover=\"alert(1)", "x")]
  = " evil_onmouseover__alert_1_=\"x\""  -- disallowed chars in the name are sanitized, not passed through
#guard renderRawAttrs [("   ", "x")] = " ___=\"x\""  -- each disallowed char becomes its own "_"
#guard renderRawAttrs [("", "x")] = " _=\"x\""  -- an empty name sanitizes to nothing, falls back to "_"
-- A digit, `-`, and `.` are legal in an attribute name but not as its first
-- character, so a name starting with one gains a `_` rather than losing it.
#guard renderRawAttrs [("1st", "x")] = " _1st=\"x\""
#guard renderRawAttrs [("-webkit", "x")] = " _-webkit=\"x\""
#guard renderRawAttrs [("data-1", "x")] = " data-1=\"x\""

-- In XML every attribute carries a value, boolean flags included.
#guard (InputAttrs.render { checked := true }).render .xhtml
  = " type=\"text\" checked=\"checked\""

end HtmlTests

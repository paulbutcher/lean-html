/-
Copyright (c) 2026 Paul Butcher. All rights reserved.
Released under Apache 2.0 license as described in the file LICENSE.
-/
module

@[expose] public section

namespace Html

/-- Which serialization to emit. `html5` is the usual HTML5 syntax: void
elements have no closing tag (`<br>`) and boolean attributes are bare
(`disabled`). `xhtml` is XML: every element either closes (`</p>`) or
self-closes (`<br />`), and every attribute carries a value
(`disabled="disabled"`). It's a whole-document convention rather than a
per-tag or per-attribute choice, so it's a render-time parameter rather
than something recorded in the tree. -/
inductive Dialect where
  | html5
  | xhtml
  deriving DecidableEq

end Html

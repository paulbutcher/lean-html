import HtmlTests.Escape
import HtmlTests.Node
import HtmlTests.Attrs
import HtmlTests.Tags
import HtmlTests.Document

/-!
# `HtmlTests`: tests for the `Html` library

Build-time tests (`#guard`/`#guard_msgs`) for `Html.Escape` (`HtmlTests/Escape.lean`),
`Html.Node` (`HtmlTests/Node.lean`), `Html.Attrs` (`HtmlTests/Attrs.lean`), `Html.Tags`
(`HtmlTests/Tags.lean`), and `Html.Document` (`HtmlTests/Document.lean`). Not a default
target -- run via `lake test`, which for a library target simply builds it (see
the package's `testDriver` in `lakefile.toml`), triggering every `#guard` below
as a build-breaking error on failure.
-/

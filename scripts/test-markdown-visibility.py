#!/usr/bin/env python3
"""Behavior tests for rendered Markdown visibility helpers."""

from __future__ import annotations

import unittest

from markdown_visibility import rendered_markdown_with_code, visible_markdown


class MarkdownVisibilityTests(unittest.TestCase):
    def test_inline_code_comment_opener_does_not_hide_following_content(self) -> None:
        text = "Literal code: `<!--`.\n\n[Decision](../rfcs/accepted/0001.md)\n"

        visible = visible_markdown(text)

        self.assertNotIn("<!--", visible)
        self.assertIn("[Decision](../rfcs/accepted/0001.md)", visible)

    def test_backticks_inside_html_comment_do_not_hide_following_content(self) -> None:
        text = "<!-- an unmatched ` delimiter -->\n\n## Visible heading\n"

        visible = visible_markdown(text)

        self.assertNotIn("delimiter", visible)
        self.assertIn("## Visible heading", visible)

    def test_rendered_code_is_preserved_without_exposing_hidden_comment_text(
        self,
    ) -> None:
        text = """<!-- hidden grammar -->
```grammar
case-arm-pattern := pattern
```
"""

        rendered = rendered_markdown_with_code(text)

        self.assertNotIn("hidden grammar", rendered)
        self.assertIn("case-arm-pattern := pattern", rendered)


if __name__ == "__main__":
    unittest.main()

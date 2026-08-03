#!/usr/bin/env python3
"""Behavior tests for rendered Markdown visibility helpers."""

from __future__ import annotations

import unittest

from markdown_visibility import (
    rendered_markdown,
    rendered_markdown_with_code,
    visible_markdown,
)


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

    def test_html_block_openers_inside_comments_and_code_remain_inert(self) -> None:
        text = (
            "<!--\n<div>\n-->\n"
            "`<script type=\"text/plain\">`\n"
            "## Visible heading\n"
        )

        visible = visible_markdown(text)

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

    def test_commonmark_raw_html_blocks_do_not_expose_hidden_markdown(self) -> None:
        blocks = (
            '<script type="text/plain">\n## Hidden script\n</script>\n',
            "<?fixture\n## Hidden processing instruction\n?>\n",
            "<!DOCTYPE fixture>\n",
            "<![CDATA[\n## Hidden CDATA\n]]>\n",
            "<div>\n## Hidden block tag\n</div>\n\n",
            "<fixture data-kind=\"raw\">\n## Hidden custom tag\n</fixture>\n\n",
        )
        for block in blocks:
            with self.subTest(block=block.splitlines()[0]):
                text = block + "## Visible heading\n"

                visible = visible_markdown(text)
                rendered = rendered_markdown(text)

                self.assertNotIn("Hidden", visible)
                self.assertNotIn("Hidden", rendered)
                self.assertIn("## Visible heading", visible)
                self.assertIn("## Visible heading", rendered)

    def test_custom_html_block_can_follow_a_heading_without_a_blank_line(self) -> None:
        text = (
            "## Before\n"
            "<fixture>\n"
            "## Hidden custom content\n"
            "</fixture>\n\n"
            "## Visible heading\n"
        )

        visible = visible_markdown(text)

        self.assertNotIn("Hidden custom content", visible)
        self.assertIn("## Before", visible)
        self.assertIn("## Visible heading", visible)


if __name__ == "__main__":
    unittest.main()

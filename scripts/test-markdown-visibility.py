#!/usr/bin/env python3
"""Behavior tests for rendered Markdown visibility helpers."""

from __future__ import annotations

import unittest

from markdown_visibility import (
    markdown_fences,
    renderable_source_markdown,
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

    def test_renderable_source_masks_raw_html_but_preserves_example_metadata(
        self,
    ) -> None:
        text = (
            "<!-- jazz-example: fragment -->\n"
            "```jazz\n0.\n```\n"
            '<script type="text/plain">\n'
            "<!-- jazz-example: executable path=examples/hidden.jz -->\n"
            "```jazz\n1.\n```\n"
            "</script>\n"
        )

        source = renderable_source_markdown(text)

        self.assertIn("<!-- jazz-example: fragment -->", source)
        self.assertIn("```jazz\n0.\n```", source)
        self.assertNotIn("examples/hidden.jz", source)
        self.assertNotIn("```jazz\n1.\n```", source)

    def test_renderable_source_rejects_metadata_nested_in_a_larger_comment(
        self,
    ) -> None:
        text = (
            "<!--\n"
            "<!-- jazz-example: executable path=examples/hidden.jz -->\n"
            "```jazz\n1.\n```\n"
            "<!-- jazz-example-output: case=hidden -->\n"
            "```text\n1\n```\n"
            "-->\n"
        )

        source = renderable_source_markdown(text)

        self.assertNotIn("jazz-example", source)
        self.assertNotIn("```jazz", source)
        self.assertNotIn("```text", source)

    def test_renderable_source_rejects_multiline_metadata_comment(self) -> None:
        text = (
            "<!--\n"
            "jazz-example: executable path=examples/hidden.jz\n"
            "-->\n"
            "```jazz\n"
            "1.\n"
            "```\n"
        )

        source = renderable_source_markdown(text)

        self.assertNotIn("jazz-example", source)
        self.assertIn("```jazz", source)

    def test_list_continuation_indentation_is_not_executable_source(self) -> None:
        text = (
            "1. Example\n\n"
            "    ```jazz\n"
            "    first.\n"
            "    \n"
            "    second.\n"
            "    ```\n"
        )

        fences = markdown_fences(text)

        self.assertEqual(1, len(fences))
        self.assertEqual("first.\n\nsecond.\n", fences[0].source)

    def test_thematic_break_does_not_open_a_list_container(self) -> None:
        for thematic_break in ("- - -", "* * *"):
            with self.subTest(thematic_break=thematic_break):
                text = (
                    f"{thematic_break}\n"
                    "  ```text\n"
                    "  hidden code\n"
                    "```\n"
                    "## Visible heading\n"
                )

                fences = markdown_fences(text)
                visible = visible_markdown(text)

                self.assertEqual(1, len(fences))
                self.assertTrue(fences[0].closed)
                self.assertEqual("hidden code\n", fences[0].source)
                self.assertNotIn("hidden code", visible)
                self.assertIn("## Visible heading", visible)

    def test_blockquoted_raw_html_does_not_expose_markdown_contracts(self) -> None:
        text = (
            '> <script type="text/plain">\n'
            "> <!-- jazz-example: executable path=examples/hidden.jz -->\n"
            "> ```jazz\n"
            "> 1.\n"
            "> ```\n"
            "> </script>\n"
        )

        source = renderable_source_markdown(text)

        self.assertNotIn("examples/hidden.jz", source)
        self.assertNotIn("```jazz", source)


if __name__ == "__main__":
    unittest.main()

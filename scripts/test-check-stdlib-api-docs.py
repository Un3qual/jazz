#!/usr/bin/env python3
"""Regression tests for the standard-library API documentation checker."""

from __future__ import annotations

import importlib.util
import tempfile
import unittest
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
CHECKER = ROOT / "scripts" / "check-stdlib-api-docs.py"


def load_checker():
    if not CHECKER.is_file():
        raise AssertionError(f"missing checker: {CHECKER}")
    spec = importlib.util.spec_from_file_location("check_stdlib_api_docs", CHECKER)
    if spec is None or spec.loader is None:
        raise AssertionError("unable to load standard-library API checker")
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


class StandardLibraryApiDocsTests(unittest.TestCase):
    def check(self, source: str, document: str) -> list[str]:
        checker = load_checker()
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            source_path = root / "jazz" / "stdlib" / "Sample.jz"
            document_path = root / "docs" / "standard-library" / "sample.md"
            source_path.parent.mkdir(parents=True)
            document_path.parent.mkdir(parents=True)
            source_path.write_text(source, encoding="utf-8")
            document_path.write_text(document, encoding="utf-8")
            return checker.check_module(root, source_path, document_path)

    def test_accepts_documented_public_value_with_exact_signature(self) -> None:
        violations = self.check(
            """module Sample (value sampleMap) {
  sampleMap :: (a -> b) -> Sample(a) -> Sample(b).
  sampleMap = transform.
}
""",
            """# Sample

## `sampleMap`

```jazz jazz-signature
sampleMap :: (a -> b) -> Sample(a) -> Sample(b).
```

Transforms the contained value.
""",
        )
        self.assertEqual(violations, [])

    def test_reports_missing_value_heading(self) -> None:
        violations = self.check(
            """module Sample (value sampleMap) {
  sampleMap :: a -> a.
  sampleMap = value.
}
""",
            """# Sample

```jazz jazz-signature
sampleMap :: a -> a.
```
""",
        )
        self.assertIn("missing heading for value `sampleMap`", violations[0])

    def test_reports_missing_signature_fence(self) -> None:
        violations = self.check(
            """module Sample (value sampleMap) {
  sampleMap :: a -> a.
  sampleMap = value.
}
""",
            """# Sample

## `sampleMap`

Transforms a value.
""",
        )
        self.assertIn("missing exact signature for `sampleMap`", violations[0])

    def test_reports_stale_signature(self) -> None:
        violations = self.check(
            """module Sample (value sampleMap) {
  sampleMap :: a -> a.
  sampleMap = value.
}
""",
            """# Sample

## `sampleMap`

```jazz jazz-signature
sampleMap :: a -> Bool.
```
""",
        )
        self.assertIn("missing exact signature for `sampleMap`", violations[0])

    def test_requires_public_type_and_constructor_headings(self) -> None:
        violations = self.check(
            """module Sample (type Sample(..)) {
  data Sample a = Empty | Item a.
}
""",
            """# Sample

## `Sample`

### `Empty`

### `Item`
""",
        )
        self.assertEqual(violations, [])

    def test_requires_explicit_constructor_heading(self) -> None:
        violations = self.check(
            """module Sample (type Sample, constructor Sample) {
  data Sample = Sample Int.
}
""",
            """# Sample

## `Sample`
""",
        )
        self.assertIn("missing heading for constructor `Sample`", violations[0])

    def test_ignores_private_helpers(self) -> None:
        violations = self.check(
            """module Sample (value publicValue) {
  privateHelper :: a -> a.
  privateHelper = value.
  publicValue :: a -> a.
  publicValue = privateHelper.
}
""",
            """# Sample

## `publicValue`

```jazz jazz-signature
publicValue :: a -> a.
```
""",
        )
        self.assertEqual(violations, [])

    def test_rejects_exact_signature_in_an_ordinary_jazz_fence(self) -> None:
        violations = self.check(
            """module Sample (value sampleMap) {
  sampleMap :: a -> a.
  sampleMap = value.
}
""",
            """# Sample

## `sampleMap`

```jazz
sampleMap :: a -> a.
```
""",
        )
        self.assertIn("missing exact signature for `sampleMap`", violations[0])

    def test_rejects_near_match_signature_metadata(self) -> None:
        violations = self.check(
            """module Sample (value sampleMap) {
  sampleMap :: a -> a.
  sampleMap = value.
}
""",
            """# Sample

## `sampleMap`

```jazz not-jazz-signature
sampleMap :: a -> a.
```
""",
        )
        self.assertIn("missing exact signature for `sampleMap`", violations[0])


if __name__ == "__main__":
    unittest.main()

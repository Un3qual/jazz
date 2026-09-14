# Changelog

All notable changes to Jazz will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.1.0/),
and the project intends to use [Semantic Versioning](https://semver.org/spec/v2.0.0.html)
once its pre-1.0 release process is active.

## [Unreleased]

### Added

- Generic capability implementations, constructor kinds, ordinary method values,
  superclasses, and default methods (RFC 0019).
- Generic collection mapping, folding, equality, and combination; Unicode Text
  mapping; and the explicit-import Reduce module.
- Tiered pull-request, ordinary, extended, and release-candidate verification.
- Contributor, security, release, and repository-maintenance policies.

### Changed

- Renamed `Eq`/`Ord` to `Equatable`/`Comparable` and removed redundant module
  prefixes from 183 library exports. Specialized argument orders are unchanged;
  callers use final names such as `List::append` and `Map::mapValues`.
- Curated public documentation and the Docusaurus website now share one public
  documentation source.

### Fixed

- Nothing yet.

### Security

- Nothing yet.

## [0.1.0-alpha.1] - Unreleased

This section describes the first alpha being prepared. It is not a published
release.

### Added

- Initial source distribution for the Haskell compiler, interpreter-backed CLI,
  Jazz-authored standard library, checked examples, and public documentation.
- Reproducible Nix and source-build paths.
- Checked release-artifact and checksum process.

### Changed

- Nothing yet.

### Fixed

- Nothing yet.

### Security

- Nothing yet.

[Unreleased]: https://github.com/un3qual/jazz/compare/v0.1.0-alpha.1...HEAD
[0.1.0-alpha.1]: https://github.com/un3qual/jazz/compare/1df2c5bd2524cdd52bd18e989967373e255f80d3...v0.1.0-alpha.1

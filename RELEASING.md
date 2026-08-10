# Releasing Jazz

Jazz releases are explicit maintainer actions. The repository prepares and
verifies alpha artifacts, but no workflow automatically creates a tag, GitHub
release, package-registry upload, or other publication.

## Versioning

Pre-1.0 tags use `v0.<minor>.<patch>-alpha.<n>`, for example
`v0.1.0-alpha.1`. The corresponding artifact version omits the leading `v`.
Cabal requires a numeric package version, so update `version` in `jazz.cabal`
to the numeric release line and record the alpha identifier in the tag,
changelog, and release notes.

For each candidate:

1. Update `jazz.cabal` when the numeric package version changes.
2. Promote relevant entries from `Unreleased` into the candidate section in
   `CHANGELOG.md`; replace `Unreleased` with the release date only when the
   release is published.
3. Create or update `release-notes/<version>.md` with implemented behavior,
   installation instructions, known limitations, and compatibility warnings.
4. Update public status or installation documentation only when the candidate
   changes those facts. The editor extension is separately versioned and must
   not be advanced unless it is actually part of the release.

## Build and verify locally

Start from the intended release commit with a clean tracked and untracked tree.
Use the pinned Nix environment and an alpha version without the tag prefix:

```bash
git status --short
nix --extra-experimental-features 'nix-command flakes' develop --command \
  env JAZZ_RELEASE_VERSION=0.1.0-alpha.1 bash scripts/release/build-alpha.sh
python3 scripts/release/verify-artifacts.py artifacts/release/0.1.0-alpha.1
```

The build script runs the complete ordinary and extended verification tiers,
package checks, the Nix build, documentation and website checks, and exact
artifact validation. Benchmark timings are recorded as evidence; timing
percentages do not determine pass or fail.

The builder rejects a dirty worktree and an existing final artifact directory.
It gives the extended tier a fresh temporary evidence root, creates the Cabal
source archive and Nix result through fresh caller-owned output paths, exports
the Nix result's complete runtime closure in sorted store-path order, and moves
the verified set into `artifacts/release/<version>/` only after every gate
passes. Generated `artifacts/`, website output, Cabal output, and the default
Nix `result` link are ignored; none belong in a release-preparation commit.

A valid candidate directory contains exactly:

```text
jazz-<version>-source.tar.gz
jazz-<version>-nix-<system>.tar.gz
jazz-<version>-docs.tar.gz
jazz-<version>-benchmark-evidence.tar.gz
SHA256SUMS
```

Validate every archive against the generated checksums before publication:

```bash
cd artifacts/release/0.1.0-alpha.1
if command -v sha256sum >/dev/null; then
  sha256sum -c SHA256SUMS
else
  shasum -a 256 -c SHA256SUMS
fi
```

The source archive must exclude internal `.codex` state, dependency/build
output, profiles, and benchmark results, and it must include both `flake.nix`
and `flake.lock`. The docs archive must contain the static site index and pass
the same generated-output publication-boundary scan as the deployed site.
Extended evidence must contain normalized corpus output,
deterministic profiles, benchmark metadata/results, and its SHA-256 manifest.

The Nix archive is a same-system runtime closure, not a copied result tree. It
contains `nix-closure/closure.nar`, the sorted `store-paths` used for the
export, `root-store-path`, and `system`. On a machine with that same Nix system,
verify the artifact set, extract the archive, import the closure, and run the
recorded root executable:

```bash
tar -xzf jazz-0.1.0-alpha.1-nix-<system>.tar.gz
cat nix-closure/system
nix-store --import < nix-closure/closure.nar
root_store_path="$(cat nix-closure/root-store-path)"
"$root_store_path/bin/jazz" --help
```

Do not import an artifact whose recorded system differs from the target
machine. `verify-artifacts.py` checks that the recorded root is a valid member
of the sorted exported closure and that the recorded system matches the
artifact filename.

## Verify in CI

Run the **Release candidate** GitHub Actions workflow with the same version.
Its required `version` input omits the leading `v`. A pushed `v*` tag also
starts the same read-only build, deriving the candidate version from the tag.
In either case the workflow runs `scripts/release/build-alpha.sh` through the
Nix development shell and uploads the complete verified directory for 30 days.
It does not create a GitHub release, push a tag, or publish a package.

Download the workflow artifact into a directory named for the alpha version.
From a checkout of the exact source revision, run the tracked verifier against
that download directory, then verify its own `SHA256SUMS`:

```bash
python3 scripts/release/verify-artifacts.py /path/to/0.1.0-alpha.1
(
  cd /path/to/0.1.0-alpha.1
  if command -v sha256sum >/dev/null; then
    sha256sum -c SHA256SUMS
  else
    shasum -a 256 -c SHA256SUMS
  fi
)
```

Compare archives byte-for-byte only when the system and all build inputs match.
Publish one complete verified artifact set; do not combine local and CI
archives under a single checksum file.

## Publication checklist

Before creating a public release:

- confirm ordinary, extended, and release-candidate checks passed for the exact
  commit;
- review the changelog and release notes for unsupported claims;
- verify every archive and checksum from a clean checkout;
- confirm license, source, docs, and benchmark-evidence contents;
- if the documentation website is part of the announcement, confirm the Pages
  workflow succeeded for the same commit and that the published homepage,
  getting-started guide, and language reference resolve; and
- obtain an explicit maintainer publication decision.

Only then create and push the annotated tag:

```bash
git tag -a v0.1.0-alpha.1 -m "Jazz 0.1.0-alpha.1"
git push origin v0.1.0-alpha.1
```

Create the GitHub prerelease manually from that immutable tag, attach the exact
verified archives and `SHA256SUMS`, and use the tracked release notes. Do not
describe an alpha as stable or production-ready.

## Abort or recover

If any gate fails before publication, do not tag or publish. Remove the local
candidate directory, fix the cause, and rebuild the entire candidate from a
clean tree. Do not reuse partially generated archives or checksums.

If a tag was pushed but no public release was created, stop and review the
repository state before taking any destructive action. Never move or overwrite
a published tag. If faulty artifacts were published, withdraw the affected
assets or mark the prerelease as affected, document the problem, and prepare a
new alpha; never silently replace an archive while retaining its filename or
checksum.

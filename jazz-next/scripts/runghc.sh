#!/usr/bin/env bash
set -euo pipefail

if command -v runghc >/dev/null 2>&1; then
  exec runghc "$@"
fi

GHCUP_RUNGHC="${HOME}/.ghcup/bin/runghc"
if [[ -x "${GHCUP_RUNGHC}" ]]; then
  exec "${GHCUP_RUNGHC}" "$@"
fi

echo "runghc not found on PATH or at ${GHCUP_RUNGHC}" >&2
exit 127

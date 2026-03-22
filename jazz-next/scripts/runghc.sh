#!/usr/bin/env bash
set -euo pipefail

if command -v runghc >/dev/null 2>&1; then
  exec runghc "$@"
fi

GHCUP_RUNGHC=""
if [[ -n "${HOME-}" ]]; then
  GHCUP_RUNGHC="${HOME}/.ghcup/bin/runghc"
  if [[ -x "${GHCUP_RUNGHC}" ]]; then
    exec "${GHCUP_RUNGHC}" "$@"
  fi
fi

if [[ -n "${GHCUP_RUNGHC}" ]]; then
  echo "runghc not found on PATH or at ${GHCUP_RUNGHC}" >&2
else
  echo "runghc not found on PATH and HOME is unset, so ~/.ghcup/bin/runghc could not be checked" >&2
fi
exit 127

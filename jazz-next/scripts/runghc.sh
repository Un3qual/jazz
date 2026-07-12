#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
JAZZ_NEXT_DIR="$(cd "${SCRIPT_DIR}/.." && pwd)"

if [[ -z "${JAZZ_NEXT_RUNGHC_IN_CABAL-}" && "${JAZZ_NEXT_RUNGHC_NO_CABAL-}" != "1" && -f "${JAZZ_NEXT_DIR}/jazz-next.cabal" ]] && command -v cabal >/dev/null 2>&1; then
  export JAZZ_NEXT_RUNGHC_IN_CABAL=1
  if cabal exec --project-dir="${JAZZ_NEXT_DIR}" -- sh -c 'command -v runghc >/dev/null 2>&1 || exit 127'; then
    exec cabal exec --project-dir="${JAZZ_NEXT_DIR}" -- runghc "$@"
  else
    status=$?
    if [[ "${status}" -ne 127 ]]; then
      exit "${status}"
    fi
  fi
fi

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

#!/usr/bin/env bash
set -euo pipefail

if (( $# == 0 )); then
  printf 'usage: scripts/check-haskell-format.sh PATH.hs [PATH.hs ...]\n' >&2
  exit 2
fi

for source_path in "$@"; do
  case "$source_path" in
    *.hs) ;;
    *)
      printf 'not a Haskell source path: %s\n' "$source_path" >&2
      exit 2
      ;;
  esac
done

exec nix --extra-experimental-features 'nix-command flakes' develop --command ormolu --mode check "$@"

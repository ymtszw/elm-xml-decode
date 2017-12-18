#!/usr/bin/env bash
set -euo pipefail
mkdir -p docs
(
  cd benchmarks
  "$(npm bin)/elm-make" Benchmarks.elm --output ../docs/index.html
)

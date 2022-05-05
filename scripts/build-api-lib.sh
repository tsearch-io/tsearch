#!/usr/bin/env bash
set -euo pipefail

rm -rf api-lib

tie --output api-lib \
  --module-name Tsearch.API \
  --package-name tsearch-api-lib \
  api.yml

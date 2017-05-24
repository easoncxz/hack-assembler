#!/bin/bash

set -e  # fail fast
set -x  # be verbose

export PATH="$(stack path --bin-path)"

deploy-hack-assembler update-sdist
deploy-hack-assembler update-bottle

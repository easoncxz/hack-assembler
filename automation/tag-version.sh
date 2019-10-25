#!/usr/bin/env bash

set -e

stack build
git tag "$(stack exec -- hack-assembler-automation version-as-tag)"
git show -q --decorate --color | cat

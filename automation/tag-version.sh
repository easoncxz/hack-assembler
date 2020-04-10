#!/usr/bin/env bash

set -e

stack build
git tag v"$(./automation/version-as-num.sh)"  # notice the `v`
git show -q --decorate --color | cat

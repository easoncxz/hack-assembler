#!/usr/bin/env bash

stack build
git tag "$(stack exec -- hack-assembler-automation version-as-tag)"

#!/usr/bin/env bash

# Hey look, by doing this like so, I don't need to call `stack`,
# which very well might not be installed at all, or use any real
# Cabal file parser.

# Pluck out whatever comes in the second half of
# a line of `version: ...`, allowing for dash (-),
# period (.), and alpha-numeric characters as part of the version
# field. Currently I'm using only numbers and periods.
grep '^version:' hack-assembler.cabal \
  | sed 's/^version:[[:space:]]*\([-.[:alnum:]]\)/\1/'

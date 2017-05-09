#!/bin/bash

set -ev

## Upload sdist tarball to Github Releases to use as non-bottle URL
stack sdist
curl    \
    -X POST \
    -H "Authorization: token $EASONCXZ_GITHUB_OAUTH_TOKEN" \
    --data-binary @"$(stack path --dist-dir)/hack-assembler-$(./automation/version.sh hack-assembler.cabal).tar.gz" \
    -i \
    https://api.github.com/repos/easoncxz/hack-assembler/releases

## First update the Homebrew formula over at the Tap repo


## Build a Bottle


## Upload Bottle


## Update the Homebrew formula with k

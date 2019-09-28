#!/usr/bin/bash

set -e
set -x

# Why is this even needed again?
pwd
ls -l
./automation/install-homebrew-automation.sh

homebrew_automation.rb bottle gather-and-publish \
    --source-user easoncxz \
    --source-repo hack-assembler \
    --source-tag "$(stack exec -- hack-assembler-automation version-as-tag)" \
    \
    --tap-user easoncxz \
    --tap-repo homebrew-tap \
    --tap-token "$EASONCXZ_GITHUB_OAUTH_TOKEN_v2" \
    \
    --bintray-user easoncxz \
    --bintray-token "$EASONCXZ_BINTRAY_API_KEY"
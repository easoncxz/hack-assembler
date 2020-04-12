#!/usr/bin/env bash

set -x
set -e

homebrew_automation.rb bottle build-and-upload \
    --source-user easoncxz \
    --source-repo hack-assembler \
    --source-tag v"$(./automation/version-as-num.sh)" \
    \
    --tap-user easoncxz \
    --tap-repo homebrew-tap \
    --tap-token "$EASONCXZ_GITHUB_OAUTH_TOKEN_v2" \
    \
    --bintray-user easoncxz \
    --bintray-token "$EASONCXZ_BINTRAY_API_KEY"

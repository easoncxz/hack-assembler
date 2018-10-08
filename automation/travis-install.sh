#!/usr/bin/env bash

set -e  # Fail fast
set -x  # Be noisy

gem install homebrew_automation -v 0.0.8
homebrew_automation.rb help

brew doctor || true
brew update || true
which stack || brew install --verbose haskell-stack

stack --no-terminal setup
stack --no-terminal test --only-dependencies

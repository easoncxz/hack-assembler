#!/usr/bin/env bash

set -e  # Fail fast
set -x  # Be noisy

rvm get latest
rvm install 'ruby-2.5.1'    # homebrew_automation depends on certain Ruby versions
gem install homebrew_automation -v 0.0.8
homebrew_automation.rb help

brew doctor || true
brew update || true
which stack || brew install --verbose haskell-stack

stack --no-terminal setup
stack --no-terminal test --only-dependencies

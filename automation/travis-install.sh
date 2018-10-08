#!/usr/bin/env bash

set -e  # Fail fast
set -x  # Be noisy

# Bring Homebrew up to date, ...
brew doctor || true
brew update || true

# ... so that we can install GnuPG, ...
brew install gnupg || brew upgrade gnupg || which gpg2
gpg2 --keyserver hkp://pgp.mit.edu --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3

# ... so that we can verify the new RVM binary when we download it, ...
rvm get latest

# ... so that we can have a version of Ruby that works with homebrew_automation, ...
rvm osx-ssl-certs update all    # https://github.com/rubygems/rubygems.org/issues/613
rvm install 'ruby-2.5.1'

# ... so that we can install homebrew_automation.
gem update --system   # https://stackoverflow.com/questions/10246023/
gem install homebrew_automation -v 0.0.8
homebrew_automation.rb help

# And then install our Haskell build tool.
which stack || brew install --verbose haskell-stack

# Download a version of GHC and compile all our Haskell dependencies. This takes ages.
stack --no-terminal setup
stack --no-terminal test --only-dependencies

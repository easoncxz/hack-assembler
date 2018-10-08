#!/usr/bin/env bash

set -e  # Fail fast
set -x  # Be noisy

# brew install gnupg || brew upgrade gnupg || which gpg2
# gpg2 --keyserver hkp://pgp.mit.edu --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3

# travis_retry rvm get latest
# rvm osx-ssl-certs update all  # https://github.com/rubygems/rubygems.org/issues/613
# rvm install ruby-2.1.8
# rvm osx-ssl-certs update all  # how about a second time?
# travis_retry gem update --system   # https://stackoverflow.com/questions/10246023/
# travis_retry gem install bundler

# travis_retry gem install homebrew_automation

#rvm use ruby-2.3    # brew.rb: "Homebrew must be run under Ruby 2.3!"

which stack || brew install --verbose haskell-stack
stack --no-terminal setup
stack --no-terminal test --only-dependencies

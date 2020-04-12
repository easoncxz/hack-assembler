#!/usr/bin/env bash

set -e  # Fail fast
set -x  # Be noisy

# Look at our Ruby
which -a ruby
ruby --version

# Install our script
which homebrew_automation.rb || gem install homebrew_automation
homebrew_automation.rb --help
homebrew_automation.rb version

#!/usr/bin/env bash --login

set -e  # Fail fast
set -x  # Be noisy

## Bring Homebrew up to date, ...
#brew --version
#brew doctor || true
#brew list --versions -1 | sort || true
#time brew upgrade || true
#brew --version

# ... so that we can install GnuPG, ...
#brew install gnupg || brew upgrade gnupg || which -a gpg
#echo 'Expecting to have GnuPG v2.x'
#gpg --version
#gpg --keyserver hkp://pgp.mit.edu --recv-keys 409B6B1796C275462A1703113804BB82D39DC0E3

#set +x  # RVM is too noisy...
#function with_echo {
#    echo "$*";
#    # Crucially, without quotes:
#    $*
#}
#
## ... so that we can verify the new RVM binary when we download it, ...
#with_echo rvm reload
#with_echo rvm version
#with_echo rvm get latest
#with_echo rvm version
#with_echo rvm disk-usage all
#
## ... so that we can have a version of Ruby that works with homebrew_automation, ...
#with_echo cat .ruby-version
#with_echo rvm use "$(cat .ruby-version)" --install
#with_echo which -a ruby
#with_echo which -a gem
#with_echo ruby --version
#with_echo gem --version

#set -x

# ... so that we can install homebrew_automation.
gem install homebrew_automation -v 0.0.8
homebrew_automation.rb --help

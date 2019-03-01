#!/usr/bin/env bash

# Install our Haskell build tool.
which -a stack || ( curl -sSL https://get.haskellstack.org/ | sh )

# Download a version of GHC and compile all our Haskell dependencies. This takes ages.
stack --no-terminal setup
stack --no-terminal test --only-dependencies

# Get a rough idea of Travis cache size
du -h -d 3 ~/.stack
du -h -d 3 .stack-work

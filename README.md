hack-assembler
==============

An assembler for the Hack assembly language, targeting the Hack hardware 
platform developed in [nand2tetris](http://www.nand2tetris.org/).

Specifications for the Hack language can be found in a chapter of the textbook 
for that course: [(PDF) 6. Assembler](http://www.nand2tetris.org/chapters/chapter%2006.pdf)

Build and run:

    $ stack install
    $ hack-assembler < hello.asm > output.hack

Run tests:

    $ export HACK_ASSEMBLER_PROJ_DIR=$(pwd)     # accommodates a hack
    $ stack test

Configuring TravisCI:

    $ export OSX_VERSION_NAME="yosemite"    # or "el_capitan"; formatted like inside a Homebrew Formula
    $ export EASONCXZ_GITHUB_OAUTH_TOKEN='very-secret'
    $ export EASONCXZ_BINTRAY_API_KEY='quite-secret'

Configuring local-dev:

    $ export EASONCXZ_HOMEBREW_LOCAL_TAP="file://$HOME/your-homebrew-tap-repo"

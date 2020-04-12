hack-assembler
==============

[![Build Status](https://travis-ci.org/easoncxz/hack-assembler.svg?branch=master)](https://travis-ci.org/easoncxz/hack-assembler)
![Build and test](https://github.com/easoncxz/hack-assembler/workflows/Build%20and%20test/badge.svg)

An assembler for the Hack assembly language, targeting the Hack hardware 
platform developed in [nand2tetris](http://www.nand2tetris.org/).

Specifications for the Hack language can be found in a chapter of the textbook 
for that course: [(PDF) 6. Assembler](http://www.nand2tetris.org/chapters/chapter%2006.pdf)(link broken)

Build and run:

    $ stack build
    $ stack exec -- hack-assembler < hello.asm > output.hack

Run tests:

    $ stack test

#!/bin/bash

set -e  # fail fast
set -x  # be verbose

stack exec deploy-hack-assembler -- update-sdist
stack exec deploy-hack-assembler -- update-bottle

#!/usr/bin/env bash

## Display basic information about the current system

set -e  # Fail fast
set -x  # Be noisy

whoami
pwd
ls
( env | sort )

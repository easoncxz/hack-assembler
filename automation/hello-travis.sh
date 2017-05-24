#!/bin/bash

set -ex

pwd
whoami
env | sort
which ruby    && ruby --version
which rvm     && rvm --version
which gem     && gem --version
which bundle  && bundle --version
which openssl && openssl version -a

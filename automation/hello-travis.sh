#!/bin/bash

set -ex

pwd
whoami
env | sort
ruby --version
rvm --version
gem --version
bundle --version
openssl version -a

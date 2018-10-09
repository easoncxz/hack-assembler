#!/usr/bin/env bash

set -e  # Fail fast

# Restart a Travis build/job in debug mode.
# This is intended to be run on a development machine,
# not on a Travis VM/container.
#
# For docs, see:
#   https://docs.travis-ci.com/user/running-build-in-debug-mode/

if [ $# -lt 1 ]
then
    echo "Usage: ./travis-debug-build.sh JOB_ID"
    exit 1
fi

job_id="$1"

curl -s -X POST \
  -H "Content-Type: application/json" \
  -H "Accept: application/json" \
  -H "Travis-API-Version: 3" \
  -H "Authorization: token $EASONCXZ_TRAVIS_OAUTH_TOKEN" \
  -d "{\"quiet\": true}" \
  "https://api.travis-ci.org/job/${job_id}/debug"

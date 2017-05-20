#!/bin/bash

curl \
    -H "Authorization: token $EASONCXZ_GITHUB_OAUTH_TOKEN" \
    https://api.github.com/user \
    | grep -q easoncxz

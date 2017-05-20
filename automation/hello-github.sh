#!/bin/bash

curl \
    -H "Authorization: token $EASONCXZ_GITHUB_OAUTH_TOKEN_v2" \
    https://api.github.com/user \
    | grep -q easoncxz

#!/bin/bash

curl \
    -H "Authorization: token $EASONCXZ_GITHUB_OAUTH_TOKEN" \
    -i \
    https://api.github.com/user

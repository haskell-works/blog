#!/usr/bin/env bash

./run-stack.sh build

stack exec svgs rebuild

# stack exec site rebuild

# rsync -a                          \
#   --filter='P _site/'             \
#   --filter='P _cache/'            \
#   --filter='P .git/'              \
#   --filter='P .gitignore'         \
#   --filter='P .stack-work'        \
#   --delete-excluded               \
#   _site/                          \
#   LICENSE                         \
#   ../haskell-works.github.io/

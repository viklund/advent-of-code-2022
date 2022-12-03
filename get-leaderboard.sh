#!/usr/bin/env bash

curl \
    -H "Cookie: $(cat cookie)" \
    -v \
    'https://adventofcode.com/2022/leaderboard/private/view/382101.json' \
    > leaderboard


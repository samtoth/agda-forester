#!/bin/bash

trap "kill 0" EXIT

python3 -m http.server 1313 -d output &>/dev/null &

./build.sh

fswatch -o assets/ trees/ | while read num ; \
  do \
    echo "Rebuilding forest"
    time ./build.sh
    echo "Done"
    echo
  done

wait
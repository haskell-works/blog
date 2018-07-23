#!/usr/bin/env bash

case $1 in
  build)
    stack build
    ;;

  test)
    stack test
    ;;

  bench)
    stack bench
    ;;

  install)
    stack install
    ;;

  repl)
    stack repl
    ;;
esac

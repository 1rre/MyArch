#!/usr/bin/env sh

if [[ make -ne 0 ]] ; then
  exit 1
fi
erl -sname "tty_server" -eval "St = simulator:setup()"
#!/bin/sh

# This is a shell script file to run snake.scm
# in mintty on MSYS2.

set -u

finalize () {
  stty $TTY_SAVE
  exit 0
}

TTY_SAVE=`stty -g`
trap finalize SIGINT
stty -echo -icanon -iexten isig

cd `dirname "$0"`
gosh snake.scm

finalize


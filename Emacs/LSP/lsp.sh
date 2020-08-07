#!/usr/bin/env bash

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

NAME=$1
shift
COMMAND=(bash -c "$*")

if [[ -e /.dockerenv ]]; then
  "${COMMAND[@]}"
else
  if [[ $(realpath /code) != $(realpath $SCRIPT_DIR/../..) ]]; then
    echo >&2 "To run $NAME you must have a symlink from /code to your repo. You can make one with:"
    echo >&2
    echo >&2 "  sudo ln -s $(realpath $SCRIPT_DIR/../..) /code"
    echo >&2
    echo >&2 "Then reload the project from /code/.editor/.sublime-project."
    exit 1
  fi

  if [[ -z $(docker ps --filter=name=language_server -q) ]]; then
    # Make sure we have the right paths set
    source ~/.bashrc || source ~/.bash_profile
    $SCRIPT_DIR/argo run --misc -d --misc --name=language_server sleep infinity
  fi

  docker exec -i -u user -w /code language_server "${COMMAND[@]}"
fi

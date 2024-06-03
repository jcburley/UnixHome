#!/bin/bash

DIR="${HOME}/bin"

if [ $# -gt 1 -a \( "$1" = "-t" -o "$1" = "--to" \) ]; then
    shift
    DIR="$1"
    shift
elif [ "$1" = "-S" -o "$1" = "--system" ]; then
    shift
    DIR=/usr/local/bin
fi

if [ $# -gt 0 -o -z "${TOOLS}" ]; then
    echo >&2 "Usage: TOOLS=\"<tool1> <tool2> ...\" $0 [-S|--system] | [-t|--to <target-directory>]"
    echo >&2 "  -S or --system is like --to /usr/local/bin;"
    echo >&2 "  else, the default target-directory is ${HOME}/bin"
    exit 99
fi

mkdir -p ${DIR}
install -m a=rx -p -S ${TOOLS} ${DIR}

#!/bin/sh

if ! hash sbt ; then
    echo "Please ensure sbt is on your PATH" >&2
    exit 1
fi

args="$@"
if hash sbt-client 2>&1 >/dev/null; then
    # Use https://github.com/cb372/sbt-client/ if available
    sbt-client "cli ${args}"
elif hash sbtn 2>&1 >/dev/null; then
    # Use sbtn if available
    sbtn "cli ${args}"
else
    sbt "cli ${args}"
fi

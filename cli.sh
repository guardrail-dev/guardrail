#!/bin/sh

if ! hash sbt ; then
    echo "Please ensure sbt is on your PATH" >&2
    exit 1
fi

args="$@"
sbt "cli ${args}"

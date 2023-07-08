#!/bin/bash

EXPECTED_OUTPUT="$1"
EXE_NAME="$2"

TEMPFILE=$(mktemp)
"$EXE_NAME" > "$TEMPFILE" || exit 1
DIFF_OUTPUT=$(diff --color=always -u "$EXPECTED_OUTPUT" "$TEMPFILE")
if [ $? -ne 0 ]; then
    if [ -z ${UPDATE_EXPECT+x} ]; then
        echo "$DIFF_OUTPUT"
        exit 1
    else
        cat "$TEMPFILE" > "$EXPECTED_OUTPUT"
    fi
fi

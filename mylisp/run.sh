#!/usr/bin/env bash
# Concatenate the my-lisp sources and run (my-lisp has no require/load).
cd "$(dirname "$0")/.."
cat mylisp/mylisp-lib.my mylisp/mylisp-brahmanda.my mylisp/main.my > /tmp/wsm24-run.my
exec "${MYLISP_BIN:-/home/agents/GitHub/my-lisp/target/debug/my-lisp}" /tmp/wsm24-run.my

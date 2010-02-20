#!/bin/sh

[ -z ${FORSE_COOKIE} ] && export FORSE_COOKIE="42"
[ -z ${FORSE_NS} ] && export FORSE_NS="foo@$(hostname)"

gui="$(basename $0)"

PYTHONPATH="src/forse" python -m ${gui#start_}.Main

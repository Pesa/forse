#!/bin/sh

. ./environment

export ERL FORSE_COOKIE FORSE_NS PYTHONPATH

gui="$(basename $0)"
"${PYTHON}" -m ${gui#start_}.Main

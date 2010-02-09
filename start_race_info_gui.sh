#!/bin/sh

export FORSE_NS="${1:-foo}"
PYTHONPATH=".:src/forse" python src/forse/race_info/Main.py

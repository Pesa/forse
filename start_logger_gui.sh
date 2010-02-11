#!/bin/sh

export FORSE_NS="${1:-foo}"
PYTHONPATH="src/forse" python -m logger.Main

#!/bin/sh

. ./environment

"${ERL}" \
	-config forse \
	-pa ebin \
	-setcookie ${FORSE_COOKIE} \
	-sname "forse_shell@$(hostname)" \
	-eval 'node_manager:start(), node_manager:configure([]).'

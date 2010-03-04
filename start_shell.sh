#!/bin/sh

erl -config forse \
	-pa ebin \
	-setcookie ${FORSE_COOKIE:-42} \
	-sname "forse_shell@$(hostname)" \
	-eval 'node_manager:start(), node_manager:configure([]).'

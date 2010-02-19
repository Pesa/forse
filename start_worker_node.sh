#!/bin/sh

ERL_LIBS="." erl \
	-config forse \
	-setcookie "${FORSE_COOKIE:-42}" \
	-sname "${1:-bar}@$(hostname)" \
	-run node_manager start_link

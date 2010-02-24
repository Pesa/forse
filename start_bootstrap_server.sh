#!/bin/sh

ERL_LIBS="." erl \
	-config forse \
	-setcookie "${FORSE_COOKIE:-42}" \
	-sname "${1:-foo}@$(hostname)" \
	-run bootstrap_server

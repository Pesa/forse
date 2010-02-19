#!/bin/sh

ERL_LIBS="." erl \
	-config forse \
	-setcookie "${FORSE_COOKIE:-42}" \
	-sname "${1:-foo}@$(hostname)" \
	-eval 'bootstrap_server:start(),
		bootstrap_server:read_config_files("examples/teams.conf", "examples/track.conf", "examples/weather.conf").'

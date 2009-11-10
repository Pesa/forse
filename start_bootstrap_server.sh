#!/bin/sh

ERL_LIBS="." erl -config forse -sname ${1:-foo} -eval \
	'bootstrap_server:start(),
	bootstrap_server:read_config_files("examples/teams.conf", "examples/track.conf", "examples/weather.conf").'

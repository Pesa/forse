#!/bin/sh

ERL_LIBS="." erl -config forse -sname ${1:-bar} \
	-run node_manager start_link

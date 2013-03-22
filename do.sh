#!/bin/sh

./rebar compile
neo_vs_smith stop
rm -rf rel/neo_vs_smith/log/erlang*
./rebar generate
neo_vs_smith start
sleep 5
grc tail -n 50 -f rel/neo_vs_smith/log/erlang.log.1

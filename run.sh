#!/bin/sh
rebar compile

erl_start="erl -pa ebin/ -pa deps/*/ebin/ -smp +Q 1100000 +P 1100000 +K true"

case $1 in
    "o" )
        $erl_start -eval "observer:start ()."
        ;;
    "a" )
        $erl_start -eval "application:start(hpap)."
        ;;
    "oa" | "ao" )
        $erl_start -eval "observer:start ()." -eval "application:start(hpap)."
        ;;
    "" )
        $erl_start
        ;;
    * )
        echo "unknown args!"
        ;;
esac
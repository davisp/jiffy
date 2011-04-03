#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),
    
    etap:plan(6),
    etap:is(jiffy:decode(<<"true">>), {ok, true}, "DEC: true -> true"),
    etap:is(jiffy:encode(true), {ok, <<"true">>}, "ENC: true -> true"),
    
    etap:is(jiffy:decode(<<"false">>), {ok, false}, "DEC: false -> false"),
    etap:is(jiffy:encode(false), {ok, <<"false">>}, "ENC: false -> false"),
    
    etap:is(jiffy:decode(<<"null">>), {ok, null}, "DEC: null -> null"),
    etap:is(jiffy:encode(null), {ok, <<"null">>}, "ENC: null -> null"),

    etap:end_tests().



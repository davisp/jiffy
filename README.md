Jiffy - JSON NIFs for Erlang
============================

A JSON parser as a NIF. This is a complete rewrite of the work I did
in EEP0018 that was based on Yajl. This new version is a hand crafted
state machine that does its best to be as quick and efficient as
possible while not placing any constraints on the parsed JSON.

Usage
-----

Jiffy's API is a drop in replacement for the EEP0018 application. The
JSON representation in Erlang is also exactly the same.

    Eshell V5.8.2  (abort with ^G)
    1> jiffy:decode(<<"{\"foo\": \"bar\"}">>).
    {ok,{[{<<"foo">>,<<"bar">>}]}}
    2> Doc = {[{foo, [<<"bing">>, 2.3, true]}]}.
    {[{foo,[<<"bing">>,2.3,true]}]}
    3> jiffy:encode(Doc).
    {ok,<<"{\"foo\":[\"bing\",2.2999999999999998224,true]}">>}


Data Format
-----------

    JSON             ->   Erlang
    null             ->   null
    true             ->   true
    false            ->   false
    1                ->   1
    1.25             ->   1.25
    []               ->   []
    [true, 1.0]      ->   [true, 1.0]
    {}               ->   {[]}
    {"foo": "bar"}   ->   {[{<<"foo">>, <<"bar">>}]}

Jiffy - JSON NIFs for Erlang
============================

A JSON parser as a NIF. This is a complete rewrite of the work I did
in EEP0018 that was based on Yajl. This new version is a hand crafted
state machine that does its best to be as quick and efficient as
possible while not placing any constraints on the parsed JSON.

Usage
-----

Jiffy is a simple API. The only thing that might catch you off guard
is that the return type of jiffy:encode/1 is an iolist even though it
returns a binary most of the time.

A quick note on unicode. Jiffy only understands utf-8 in binaries. End
of story. Also, there is a jiffy:encode/2 that takes a list of options
for encoding. Currently the only supported option is `uescape`.

Errors are raised as exceptions.

    Eshell V5.8.2  (abort with ^G)
    1> jiffy:decode(<<"{\"foo\": \"bar\"}">>).
    {[{<<"foo">>,<<"bar">>}]}
    2> Doc = {[{foo, [<<"bing">>, 2.3, true]}]}.
    {[{foo,[<<"bing">>,2.3,true]}]}
    3> jiffy:encode(Doc).
    <<"{\"foo\":[\"bing\",2.3,true]}">>


Data Format
-----------

    Erlang                          JSON            Erlang
    ==========================================================================

    null                       -> null           -> null
    true                       -> true           -> true
    false                      -> false          -> false
    "hi"                       -> [104, 105]     -> [104, 105]
    <<"hi">>                   -> "hi"           -> <<"hi">>
    hi                         -> "hi"           -> <<"hi">>
    1                          -> 1              -> 1
    1.25                       -> 1.25           -> 1.25
    []                         -> []             -> []
    [true, 1.0]                -> [true, 1.0]    -> [true, 1.0]
    {[]}                       -> {}             -> {[]}
    {[{foo, bar}]}             -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
    {[{<<"foo">>, <<"bar">>}]} -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}

Improvements over EEP0018
-------------------------

Jiffy should be in all ways an improvemnt over EEP0018. It no longer
imposes limits on the nesting depth. It is capable of encoding and
decoding large numbers and it does quite a bit more checking for validity
of valid UTF-8 in strings.


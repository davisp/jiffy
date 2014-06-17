Jiffy - JSON NIFs for Erlang
============================

A JSON parser as a NIF. This is a complete rewrite of the work I did
in EEP0018 that was based on Yajl. This new version is a hand crafted
state machine that does its best to be as quick and efficient as
possible while not placing any constraints on the parsed JSON.

[![Build Status](https://travis-ci.org/davisp/jiffy.svg?branch=master)](https://travis-ci.org/davisp/jiffy)

Usage
-----

Jiffy is a simple API. The only thing that might catch you off guard
is that the return type of `jiffy:encode/1` is an iolist even though
it returns a binary most of the time.

A quick note on unicode. Jiffy only understands UTF-8 in binaries. End
of story.

Errors are raised as exceptions.

    Eshell V5.8.2  (abort with ^G)
    1> jiffy:decode(<<"{\"foo\": \"bar\"}">>).
    {[{<<"foo">>,<<"bar">>}]}
    2> Doc = {[{foo, [<<"bing">>, 2.3, true]}]}.
    {[{foo,[<<"bing">>,2.3,true]}]}
    3> jiffy:encode(Doc).
    <<"{\"foo\":[\"bing\",2.3,true]}">>

`jiffy:decode/1,2`
------------------

* `jiffy:decode(IoData)`
* `jiffy:decode(IoData, Options)`

The options for decode are:

* `{bytes_per_iter, N}` where N &gt;= 0 - This controls the number of
  bytes that Jiffy will process before yielding back to the VM. The
  mechanics of this yield are completely hidden from the end user.
* `return_maps` - Tell Jiffy to return objects using the maps data type
  on VMs that support it. This raises an error on VMs that don't support
  maps.

`jiffy:encode/1,2`
------------------

* `jiffy:encode(EJSON)`
* `jiffy:encode(EJSON, Options)`

where EJSON is a valid representation of JSON in Erlang according to
the table below.

The options for encode are:

* `uescape` - Escapes UTF-8 sequences to produce a 7-bit clean output
* `pretty` - Produce JSON using two-space indentation
* `force_utf8` - Force strings to encode as UTF-8 by fixing broken
  surrogate pairs and/or using the replacement character to remove
  broken UTF-8 sequences in data.
* `{bytes_per_iter, N}` where N &gt;= 0 - This controls the number of
  bytes that Jiffy will generate before yielding back to the VM. The
  mechanics of this yield are completely hidden from the end user.

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
    #{<<"foo">> => <<"bar">>}  -> {"foo": "bar"} -> #{<<"foo">> -> <<"bar">>}

N.B. The last entry in this table is only valid for VM's that support
the `maps` data type (i.e., 17.0 and newer) and client code must pass
the `return_maps` option to `jiffy:decode/2`.

Improvements over EEP0018
-------------------------

Jiffy should be in all ways an improvemnt over EEP0018. It no longer
imposes limits on the nesting depth. It is capable of encoding and
decoding large numbers and it does quite a bit more validation of UTF-8 in strings.


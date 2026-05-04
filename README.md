Jiffy - JSON NIFs for Erlang
============================

Jiffy is a JSON NIF library that focuses on correctness over performance. It’s
not the fastest JSON library for Erlang in standard benchmarks, but it
endeavors to be as fast as possible while affecting total system performance as
little as possible.

![Build Status](https://github.com/davisp/jiffy/actions/workflows/ci.yml/badge.svg)

Usage
-----

Jiffy is a simple API. The only thing that might catch you off guard
is that the return type of `jiffy:encode/1` is an iolist even though
it returns a binary most of the time.

A quick note on unicode. Jiffy only understands UTF-8 in binaries. End
of story.

Errors are raised as error exceptions.

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

* `return_maps` - Tell Jiffy to return objects using the maps data type
  on VMs that support it. This raises an error on VMs that don't support
  maps.
* `{null_term, Term}` - Returns the specified `Term` instead of `null`
  when decoding JSON. This is for people that wish to use `undefined`
  instead of `null`.
* `use_nil` - Returns the atom `nil` instead of `null` when decoding
  JSON. This is a short hand for `{null_term, nil}`.
* `return_trailer` - If any non-whitespace is found after the first
  JSON term is decoded the return value of decode/2 becomes
  `{has_trailer, FirstTerm, RestData::iodata()}`. This is useful to
  decode multiple terms in a single binary.
* `dedupe_keys` - If a key is repeated in a JSON object this flag
  will ensure that the parsed object only contains a single entry
  containing the last value seen. This mirrors the parsing beahvior
  of virtually every other JSON parser.
* `copy_strings` - Normally, when strings are decoded, they are
  created as sub-binaries of the input data. With some workloads, this
  leads to an undesirable bloating of memory: Strings in the decode
  result keep a reference to the full JSON document alive. Setting
  this option will instead allocate new binaries for each string, so
  the original JSON document can be garbage collected even though
  the decode result is still in use.
* `{bytes_per_red, N}` where N &gt;= 0 - This controls the number of
  bytes that Jiffy will process as an equivalent to a reduction. Each
  20 reductions we consume 1% of our allocated time slice for the current
  process. When the Erlang VM indicates we need to return from the NIF.
* `{bytes_per_iter, N}` where N &gt;= 0 - Backwards compatible option
  that is converted into the `bytes_per_red` value.

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
* `use_nil` - Encodes the atom `nil` as `null`.
* `escape_forward_slashes` - Escapes the `/` character which can be
  useful when encoding URLs in some cases.
* `{bytes_per_red, N}` - Refer to the decode options
* `{bytes_per_iter, N}` - Refer to the decode options

Pre-encoded JSON
----------------

A `{json, IoData}` tuple can appear anywhere a JSON value is expected (except
as an object key). The `IoData` is spliced into the output as is. Jiffy does
not parse, validate, copy, or pretty-print it.

    1> jiffy:encode([1, {json, <<"{\"cached\":true}">>}, 3]).
    <<"[1,{\"cached\":true},3]">>
    2> jiffy:encode({[{<<"a">>, {json, [<<"[1,">>, "2,3]"]}}]}).
    <<"{\"a\":[1,2,3]}">>

The caller is responsible for ensuring it is well-formed JSON.

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
    {[{123, bar}]}             -> {"123": "bar"} -> {[{<<"123">>, <<"bar">>}]}
    {[{1.5, bar}]}             -> {"1.5": "bar"} -> {[{<<"1.5">>, <<"bar">>}]}
    {[{<<"foo">>, <<"bar">>}]} -> {"foo": "bar"} -> {[{<<"foo">>, <<"bar">>}]}
    #{<<"foo">> => <<"bar">>}  -> {"foo": "bar"} -> #{<<"foo">> => <<"bar">>}
    #{123 => <<"bar">>}        -> {"123": "bar"} -> #{<<"123">> => <<"bar">>}
    #{1.5 => <<"bar">>}        -> {"1.5": "bar"} -> #{<<"1.5">> => <<"bar">>}

N.B. The last three entries in this table are only valid for VM's that support
the `maps` data type (i.e., 17.0 and newer) and client code must pass
the `return_maps` option to `jiffy:decode/2`.


Scheduler Usage
----

Jiffy specifically avoids using shared resources like the dirty schedulers and
instead focuses on working with Erlang’s native scheduling paradigm.

As the concurrency increases it should degrade gracefully in proportion to the
applied load. This is not a trivial task to accomplish in a NIF, in general

The `bench_scheduling.sh` benchmark in https://github.com/nickva/bench runs
concurrent JSON encoding and decoding scaled by the number of schedulers.
Testing with a few Erlang json libraries shows something like this:

```
./bench_scheduling.sh
...
scheduler responsiveness check
  input:       citm-catalog.json duration: 2000
  schedulers:  12 online
  impls:       json, jiffy, simdjsone, jsone, jsx

[json]
  1x encdec       n=84 p50=135.0ms p95=182.9ms p99=191.9ms max=196.7ms
  12x encdec      n=86 p50=129.7ms p95=189.9ms p99=203.0ms max=206.2ms
  24x encdec      n=87 p50=263.0ms p95=461.2ms p99=506.1ms max=527.1ms

[jiffy]
  1x encdec       n=309 p50=38.3ms p95=51.9ms p99=57.4ms max=66.5ms
  12x encdec      n=300 p50=41.2ms p95=52.5ms p99=59.7ms max=66.2ms
  24x encdec      n=306 p50=80.2ms p95=111.8ms p99=118.8ms max=140.1ms

[simdjsone]
  1x encdec       n=20 p50=690.1ms p95=784.6ms p99=784.6ms max=784.8ms
  12x encdec      n=16 p50=790.9ms p95=887.5ms p99=887.5ms max=899.9ms
  24x encdec      n=24 p50=1448.4ms p95=1876.7ms p99=1879.5ms max=1882.7ms

[jsone]
  1x encdec       n=60 p50=213.1ms p95=261.8ms p99=263.9ms max=264.8ms
  12x encdec      n=60 p50=204.9ms p95=329.8ms p99=345.0ms max=350.9ms
  24x encdec      n=52 p50=440.1ms p95=700.3ms p99=773.3ms max=817.3ms

[jsx]
  1x encdec       n=24 p50=398.8ms p95=539.0ms p99=544.1ms max=548.3ms
  12x encdec      n=24 p50=391.5ms p95=684.9ms p99=687.0ms max=689.6ms
  24x encdec      n=24 p50=1181.3ms p95=1479.0ms p99=1558.1ms max=1654.7ms
```

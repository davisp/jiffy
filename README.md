Jiffy - JSON NIF for Erlang
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

A quick note on Unicode. Jiffy only understands UTF-8 in binaries. End
of story.

Errors are raised as error exceptions.

    Eshell V15.2.7.13 (press Ctrl+G to abort, type help(). for help)
    1> jiffy:decode(~'{"foo": "bar"}').
    {[{<<"foo">>,<<"bar">>}]}
    2> Doc = {[{foo, [~"bing", 2.3, true]}]}.
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
  containing the last value seen. This mirrors the parsing behavior
  of virtually every other JSON parser.
* `copy_strings` - Normally, when strings are decoded, they are
  created as sub-binaries of the input data. With some workloads, this
  leads to an undesirable bloating of memory: Strings in the decode
  result keep a reference to the full JSON document alive. Setting
  this option will instead allocate new binaries for each string, so
  the original JSON document can be garbage collected even though
  the decode result is still in use.
* `{max_depth, N}` - Decode top `N` levels of nested objects and arrays.
  Objects and arrays nested deeper than that are returned as `{json, Binary}`,
  with `Binary` being their JSON. See [Partial decoding](#partial-decoding)
  below.
* `{bytes_per_red, N}` where N >= 0 - This controls the number of
  bytes that Jiffy will process as an equivalent to a reduction. The
  default is 10. Jiffy reports the reductions it used to the Erlang VM and
  yields after a time slice worth of them (4000).
* `{bytes_per_iter, N}` where N >= 0 - Backwards compatible option
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
not parse, validate, copy, or pretty-print it. Encode options such as `uescape`
do not apply to it.

    1> jiffy:encode([1, {json, ~'{"cached":true}'}, 3]).
    <<"[1,{\"cached\":true},3]">>
    2> jiffy:encode({[{~"a", {json, [~"[1,", "2,3]"]}}]}).
    <<"{\"a\":[1,2,3]}">>

The caller is responsible for ensuring it is well-formed JSON.

Partial decoding
----------------

With the `{max_depth, N}` decode option only the top `N` levels of nested
objects and arrays are decoded. Anything nested deeper is validated and
returned as `{json, Binary}`. The shape of the partial decoding is meant to be
used as pre-encoded JSON. In other words, both can be used in a forwarding
scheme, where the top levels contain info to make routing decisions without
needing to decode and re-encode potentially large nested message bodies:

    1> Doc = ~'{"id":"a1","tags":["x","y"],"body":{"k":[1,2]}}'.
    2> jiffy:decode(Doc, [return_maps, {max_depth, 1}]).
    #{<<"body">> => {json,<<"{\"k\":[1,2]}">>},
      <<"id">> => <<"a1">>,
      <<"tags">> => {json,<<"[\"x\",\"y\"]">>}}

`Binary` is a sub-binary of the input, or a copy with `copy_strings`.

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

Jiffy specifically avoids using shared resources like the dirty CPU schedulers,
since those are used for large heap garbage collection, crypto functions, large
binary matching, etc. Instead, it works with Erlang's regular VM schedulers and
yields appropriately after consuming a fraction of available reductions.
Yielding behavior can be explicitly controlled via the `{bytes_per_red, N}`
option.

To get an idea of how this works, use the `bench_scheduling.sh` benchmark from
https://github.com/nickva/bench. It checks concurrent encoding and decoding
scaled by the number of schedulers. An example run comparing against other JSON
libraries may look like:

```
./bench_scheduling.sh
...
scheduler responsiveness check
  input:       citm-catalog.json duration: 2000
  schedulers:  80 online
  impls:       json, jiffy, glazer, jsone, jsx

[json]
  1x encdec       n=55 p50=18.4ms p95=19.9ms p99=20.5ms max=24.6ms
  80x encdec      n=480 p50=182.4ms p95=234.1ms p99=247.0ms max=280.2ms
  160x encdec     n=464 p50=420.6ms p95=517.6ms p99=572.0ms max=618.5ms

[jiffy]
  1x encdec       n=122 p50=8.6ms p95=9.2ms p99=9.9ms max=15.0ms
  80x encdec      n=1430 p50=53.2ms p95=104.8ms p99=121.1ms max=145.0ms
  160x encdec     n=1314 p50=114.3ms p95=228.3ms p99=261.0ms max=310.3ms

[glazer]
  1x encdec       n=316 p50=3.0ms p95=3.7ms p99=6.1ms max=11.7ms
  80x encdec      n=236 p50=413.7ms p95=439.0ms p99=453.4ms max=459.6ms
  160x encdec     n=160 p50=976.8ms p95=1449.3ms p99=1451.1ms max=1451.6ms

[jsone]
  1x encdec       n=36 p50=27.3ms p95=34.1ms p99=36.8ms max=37.8ms
  80x encdec      n=325 p50=240.0ms p95=313.4ms p99=332.8ms max=363.1ms
  160x encdec     n=320 p50=502.1ms p95=654.0ms p99=697.8ms max=797.1ms

[jsx]
  1x encdec       n=15 p50=56.6ms p95=76.2ms p99=76.2ms max=83.2ms
  80x encdec      n=156 p50=673.3ms p95=764.0ms p99=779.7ms max=800.1ms
  160x encdec     n=150 p50=1789.0ms p95=1955.6ms p99=1983.9ms max=1999.8ms
```

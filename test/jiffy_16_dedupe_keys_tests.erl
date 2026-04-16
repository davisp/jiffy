% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_16_dedupe_keys_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

% Duplicate keys with `return_maps`. We settled on
% last value wins semantics in such cases so test that
% we preserve that
%
duplicate_keys_maps_test_() ->
    Opts = [return_maps],
    Cases = [
        % Simple duplicate
        {
            <<"{\"a\":1,\"a\":2}">>,
            #{<<"a">> => 2}
        },
        % Duplicate with other keys
        {
            <<"{\"a\":1,\"b\":2,\"a\":3}">>,
            #{<<"a">> => 3, <<"b">> => 2}
        },
        % Triple duplicate
        {
            <<"{\"x\":1,\"x\":2,\"x\":3}">>,
            #{<<"x">> => 3}
        },
        % Duplicate in nested object
        {
            <<"{\"outer\":{\"k\":1,\"k\":2}}">>,
            #{<<"outer">> => #{<<"k">> => 2}}
        },
        % No duplicates
        {
            <<"{\"a\":1,\"b\":2,\"c\":3}">>,
            #{<<"a">> => 1, <<"b">> => 2, <<"c">> => 3}
        }
    ],
    {"Test duplicate keys with maps", lists:map(fun({Json, Result}) ->
        ?_assertEqual(Result, jiffy:decode(Json, Opts))
    end, Cases)}.

dedupe_keys_test_() ->
    Opts = [dedupe_keys],
    Cases = [
        % Simple sanity check
        {
            {[{<<"foo">>, 1}]},
            {[{<<"foo">>, 1}]}
        },
        % Basic test
        {
            {[{<<"foo">>, 1}, {<<"foo">>, 2}]},
            {[{<<"foo">>, 2}]}
        },
        % Non-repeated keys are fine
        {
            {[{<<"foo">>, 1}, {<<"bar">>, 2}]},
            {[{<<"foo">>, 1}, {<<"bar">>, 2}]}
        },
        % Key order stays the same other than deduped keys
        {
            {[{<<"bar">>, 1}, {<<"foo">>, 2}, {<<"baz">>, 3}, {<<"foo">>, 4}]},
            {[{<<"bar">>, 1}, {<<"baz">>, 3}, {<<"foo">>, 4}]}
        },
        % Multiple repeats are handled
        {
            {[{<<"foo">>, 1}, {<<"foo">>, 2}, {<<"foo">>, 3}]},
            {[{<<"foo">>, 3}]}
        },
        % Sub-objects are covered
        {
            {[{<<"foo">>, {[{<<"bar">>, 1}, {<<"bar">>, 2}]}}]},
            {[{<<"foo">>, {[{<<"bar">>, 2}]}}]}
        },
        % Objects in arrays are handled
        {
            [{[{<<"foo">>, 1}, {<<"foo">>, 2}]}],
            [{[{<<"foo">>, 2}]}]
        },
        % Embedded NULL bytes are handled
        {
            {[{<<"foo\\u0000bar">>, 1}, {<<"foo\\u0000baz">>, 2}]},
            {[{<<"foo\\u0000bar">>, 1}, {<<"foo\\u0000baz">>, 2}]}
        },
        % Can dedupe with embedded NULL bytes
        {
            {[{<<"foo\\u0000bar">>, 1}, {<<"foo\\u0000bar">>, 2}]},
            {[{<<"foo\\u0000bar">>, 2}]}
        }
    ],
    {"Test dedupe_keys", lists:map(fun({Data, Result}) ->
        Json = jiffy:encode(Data),
        ?_assertEqual(Result, jiffy:decode(Json, Opts))
    end, Cases)}.

dedupe_keys_empty_test() ->
    ?assertEqual({[]}, jiffy:decode(<<"{}">>, [dedupe_keys])).

% Exercise the heap-allocated dedupe hash table path when count > 64 and hit
% more than HT_STACK_SLOTS. We're padding those coverage stats here, really.
dedupe_keys_large_test_() ->
    N = 100,
    KV = fun(I) -> [<<"\"">>, i2b(I), <<"\":">>, i2b(I)] end,
    Body = iol2b(lists:join(",", [KV(I) || I <- lists:seq(1, N)])),
    Dupes = iol2b(lists:join(",", [KV(I) || I <- lists:seq(1, N)])),
    JUnique = <<"{", Body/binary, "}">>,
    JDupes = <<"{", Body/binary, ",", Dupes/binary, "}">>,
    [
        {"Unique large object",
            fun() ->
                {Pairs} = jiffy:decode(JUnique, [dedupe_keys]),
                ?assertEqual(N, length(Pairs))
            end},
        {"All keys duplicated once",
            fun() ->
                {Pairs} = jiffy:decode(JDupes, [dedupe_keys]),
                ?assertEqual(N, length(Pairs))
            end}
    ].

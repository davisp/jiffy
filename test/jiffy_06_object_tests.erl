% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_06_object_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


object_success_test_() ->
    [gen(ok, Case) || Case <- cases(ok)].


object_failure_test_() ->
    [gen(error, Case) || Case <- cases(error)].


nested_object_test_() ->
    Obj = nested(256),
    Enc = enc(Obj),
    ?_assertEqual(Obj, dec(Enc)).


nested(0) -> <<"bottom">>;
nested(N) -> {[{to_bin(N), nested(N - 1)}]}.


to_bin(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N)).


gen(ok, {J, E}) ->
    gen(ok, {J, E, J});
gen(ok, {J1, E, J2}) ->
    {msg("~s", [J1]), [
        {"Decode", ?_assertEqual(E, dec(J1))},
        {"Encode", ?_assertEqual(J2, enc(E))}
    ]};

gen(error, J) ->
    {msg("Error: ~s", [J]), [
        ?_assertError(_, dec(J))
    ]}.


cases(ok) ->
    [
        {<<"{}">>, {[]}},
        {<<"{\"foo\": \"bar\"}">>,
            {[{<<"foo">>, <<"bar">>}]},
            <<"{\"foo\":\"bar\"}">>},
        {<<"\n\n{\"foo\":\r \"bar\",\n \"baz\"\t: 123 }">>,
            {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
            <<"{\"foo\":\"bar\",\"baz\":123}">>}
    ];

cases(error) ->
    [
        <<"{">>,
        <<"{,}">>,
        <<"{123:true}">>,
        <<"{false:123}">>,
        <<"{:\"stuff\"}">>,
        <<"{\"key\":}">>,
        <<"{\"key\": 123">>,
        <<"{\"key\": 123 true">>,
        <<"{\"key\": 123,}">>
    ].


% We put terms for large objects (> 32 keys) on the heap vs on the stack. So we
% have to ensure we test large object as well not just smaller ones to exercise
% that path.
%
empty_object_maps_test_() ->
    ?_assertEqual(#{}, dec(<<"{}">>, [return_maps])).

large_object_maps_test_() ->
    Opts = [return_maps],
    [
        {"200 keys",
            fun() ->
                {Json, Expected} = large_obj(200),
                round_trip(Json, Expected, Opts)
            end},
        {"20 duplicate keys (10 as 0, 10 as 1)",
            fun() ->
                KVs = [{I rem 2, I} || I <- lists:seq(1, 20)],
                {Json, Expected} = large_obj_kvs(KVs),
                round_trip(Json, Expected, Opts)
            end},
        {"25 keys",
            fun() ->
                {Json, Expected} = large_obj(25),
                round_trip(Json, Expected, Opts)
            end},
        {"100 keys",
            fun() ->
                {Json, Expected} = large_obj(100),
                round_trip(Json, Expected, Opts)
            end},
        {"1000 keys",
            fun() ->
                {Json, Expected} = large_obj(1000),
                round_trip(Json, Expected, Opts)
            end},
        {"100 duplicate keys (50 as 0, 50 as 1)",
            fun() ->
                KVs = [{I rem 2, I} || I <- lists:seq(1, 100)],
                {Json, Expected} = large_obj_kvs(KVs),
                round_trip(Json, Expected, Opts)
            end}
    ].

round_trip(Json, Expected, Opts) ->
    Decoded = dec(Json, Opts),
    ?assertEqual(Expected, Decoded),
    Json1 = enc(Decoded),
    Decoded1 = dec(Json1, Opts),
    ?assertEqual(Expected, Decoded1).

large_obj(N) when is_integer(N) ->
    large_obj_kvs([{I, I} || I <- lists:seq(0, N - 1)]).

large_obj_kvs(KVs) when is_list(KVs) ->
    KVs1 = [{"k" ++ integer_to_list(K), V} || {K, V} <- KVs],
    Chunks = [io_lib:format("\"~s\":~B", [K, V]) || {K, V} <- KVs1],
    Json = iolist_to_binary(["{", lists:join(",", Chunks), "}"]),
    Expected = maps:from_list([{list_to_binary(K), V} || {K, V} <- KVs1]),
    {Json, Expected}.

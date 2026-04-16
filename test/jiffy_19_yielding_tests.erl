% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_19_yielding_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

decode_bytes_per_red_test() ->
    ?assertEqual([1,2,3], dec(<<"[1,2,3]">>, [{bytes_per_red, 50}])).

encode_bytes_per_red_test() ->
    ?assertEqual(<<"[1,2,3]">>, enc([1,2,3], [{bytes_per_red, 50}])).

decode_bytes_per_iter_test() ->
    ?assertEqual([1,2,3], dec(<<"[1,2,3]">>, [{bytes_per_iter, 20000}])).

encode_bytes_per_iter_test() ->
    ?assertEqual(<<"[1,2,3]">>, enc([1,2,3], [{bytes_per_iter, 20000}])).

decode_both_bytes_opts_test() ->
    Opts = [{bytes_per_red, 100}, {bytes_per_iter, 200000}],
    ?assertEqual(1, dec(<<"1">>, Opts)).

encode_both_bytes_opts_test() ->
    Opts = [{bytes_per_red, 100}, {bytes_per_iter, 200000}],
    ?assertEqual(<<"1">>, enc(1, Opts)).

% Give it something large to chew on so it hopefully does some yielding.
% Noticed with coverage enabled we didn't check some yielding paths in CI tests
% otheriwse.
%
decode_large_with_bytes_per_red_test() ->
    Data = iol2b([
        <<"[">>,
        lists:join(<<",">>, [<<"1">> || _ <- lists:seq(1, 500)]),
        <<"]">>
    ]),
    Result = dec(Data, [{bytes_per_red, 20}]),
    ?assertEqual(500, length(Result)).

encode_large_with_bytes_per_red_test() ->
    Data = lists:duplicate(500, 1),
    Encoded = enc(Data, [{bytes_per_red, 20}]),
    ?assertEqual(Data, dec(Encoded)).

% Nested object but a large binary at the bottom. We want the yield to fire
% while the term stack holds a large (> SMALL_TERMSTACK_SIZE) entries. Then
% small byte_per_red to force yielding.
encode_deep_nesting_yield_test() ->
    Depth = 12,
    Big = binary:copy(<<"a">>, 10000),
    Seq = lists:seq(1, Depth),
    Nested = lists:foldl(fun(_, Acc) -> {[{<<"k">>, Acc}]} end, Big, Seq),
    Encoded = enc(Nested, [{bytes_per_red, 1}]),
    ?assertEqual(Nested, dec(Encoded)).

% Force yielding and test restore/save and schedule bits.
decode_excessive_yield_test_() ->
    Data = iol2b([
        <<"[">>,
        lists:join(<<",">>, [i2b(I) || I <- lists:seq(1, 2000)]),
        <<"]">>
    ]),
    Expected = lists:seq(1, 2000),
    [
        {"bytes_per_red = 1",
            ?_assertEqual(Expected, dec(Data, [{bytes_per_red, 1}]))},
        {"bytes_per_red = 0 (clamped to 1)",
            ?_assertEqual(Expected, dec(Data, [{bytes_per_red, 0}]))},
        {"bytes_per_iter = 1",
            ?_assertEqual(Expected, dec(Data, [{bytes_per_iter, 1}]))}
    ].

encode_excessive_yield_test_() ->
    Data = lists:seq(1, 2000),
    [
        {"bytes_per_red = 1",
            fun() ->
                Enc = enc(Data, [{bytes_per_red, 1}]),
                ?assertEqual(Data, dec(Enc))
            end},
        {"bytes_per_red = 0 (clamped to 1)",
            fun() ->
                Enc = enc(Data, [{bytes_per_red, 0}]),
                ?assertEqual(Data, dec(Enc))
            end},
        {"bytes_per_iter = 1",
            fun() ->
                Enc = enc(Data, [{bytes_per_iter, 1}]),
                ?assertEqual(Data, dec(Enc))
            end}
    ].

% Single large string encoded/decoded + low yield threshold, so
% we hopefully hit the pct_used > 100 in bump_used_reds
large_string_yield_test_() ->
    Big = binary:copy(<<"abc">>, 50000),
    Json = <<"\"", Big/binary, "\"">>,
    [
        {"Decode",
            ?_assertEqual(Big, dec(Json, [{bytes_per_red, 1}]))},
        {"Encode",
            ?_assertEqual(Json, enc(Big, [{bytes_per_red, 1}]))}
    ].

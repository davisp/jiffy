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

% Before yielding first time the decoder keeps the stack inline so test
% yielding more thoroughly along with more nesting at the same time.
decode_deep_nesting_yield_test_() ->
    Depth = 200,
    Pad = binary:copy(<<" ">>, 5000),
    Open = lists:duplicate(Depth, <<"[">>),
    Close = lists:duplicate(Depth, <<"]">>),
    Nested = lists:foldl(fun(_, Acc) -> [Acc] end, 1, lists:seq(1, Depth)),
    [
        {"deep nesting then yield",
            ?_assertEqual(
                Nested,
                dec(iol2b([Open, Pad, <<"1">>, Pad, Close]), [{bytes_per_red, 1}])
            )},
        {"yield then deep nesting",
            ?_assertEqual(
                [Nested],
                dec(iol2b([<<"[">>, Pad, Open, <<"1">>, Close, <<"]">>]), [{bytes_per_red, 1}])
            )}
    ].

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

% Encoder errors out after it switched from the stack buffer to a binary but
% before it yielded. That way there is no resource to release that binary.
% (This was found from seeing missing covarge with lcov/gcov running).
encode_error_after_first_chunk_test() ->
    Big = binary:copy(<<"a">>, 3000),
    ?assertError({invalid_string, _}, enc([Big, <<255>>])).

% Bignums and pre-encoded values get handled in Erlang, and the encoder flushes
% its buffer before them: the stack buffer, or a binary. They count for a few
% bytes, so a yield can come right after one, when a long run went out with the
% buffer and there is no buffer at all. Sweep the run so the yield lands there,
% in the first call and after a yield. (This was found from seeing missing
% coverage with lcov/gcov running).
encode_yield_after_bignum_flush_test_() ->
    Big = 1 bsl 70,
    Pre = {json, <<"0">>},
    Pad = binary:copy(<<"b">>, 4100),
    Opts = [{bytes_per_red, 1}],
    Check = fun(Data) -> ?_assertEqual(Data, dec(enc(Data, Opts))) end,
    Sweep = fun(Val, Dec) ->
        {timeout, 60, fun() ->
            lists:foreach(fun(N) ->
                Aa = binary:copy(<<"a">>, N),
                ?assertEqual([Aa, Dec, 1], dec(enc([Aa, Val, 1], Opts))),
                ?assertEqual([Pad, Aa, Dec, 1], dec(enc([Pad, Aa, Val, 1], Opts)))
            end, lists:seq(3950, 4010))
        end}
    end,
    [
        {"bignum flush with stack buffer",
            Check(lists:duplicate(1004, 1) ++ [Big, 2])},
        {"bignum flush with a binary buffer",
            Check([binary:copy(<<"a">>, 2100), Big, 2])},
        {"yield right after a bignum", Sweep(Big, Big)},
        {"yield right after a pre-encoded value", Sweep(Pre, 0)}
    ].

% A document that is one long string, so the last token is all of it
single_string_uses_reductions_test_() ->
    Big = binary:copy(<<"a">>, 30000),
    Json = <<"\"", Big/binary, "\"">>,
    N = 20,
    [
        {"decode",
            ?_assert(reds(fun() -> jiffy:decode(Json) end, N) > N * 1000)},
        {"encode",
            ?_assert(reds(fun() -> jiffy:encode(Big) end, N) > N * 1000)}
    ].

% The flush before a bignum does not count the flushed bytes twice. That made
% this yield at the bignum and again right after it, for two whole time slices.
bignum_flush_reductions_test() ->
    Data = [binary:copy(<<"a">>, 21000), 1 bsl 70, 2],
    N = 20,
    ?assert(reds(fun() -> jiffy:encode(Data) end, N) < N * 3000).

% Pre-encoded values and bignums are added in Erlang, so the encoder counts
% each as a few bytes. Before a long list of them was nearly free.
pre_encoded_reductions_test_() ->
    N = 5000,
    Opts = [{bytes_per_red, 1}],
    Pres = lists:duplicate(N, {json, <<"1">>}),
    Bigs = lists:duplicate(N, 1 bsl 70),
    [
        {"pre-encoded array",
            ?_assert(reds(fun() -> jiffy:encode(Pres, Opts) end, 1) > N * 10)},
        {"bignum array",
            ?_assert(reds(fun() -> jiffy:encode(Bigs, Opts) end, 1) > N * 10)}
    ].

reds(Fun, N) ->
    {reductions, R0} = process_info(self(), reductions),
    lists:foreach(fun(_) -> Fun() end, lists:seq(1, N)),
    {reductions, R1} = process_info(self(), reductions),
    R1 - R0.

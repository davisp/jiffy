% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_18_pre_encoded_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

bare_pre_encoded_test() ->
    ?assertEqual(<<"[1,2,3]">>, enc({json, <<"[1,2,3]">>})),
    ?assertEqual(<<"42">>, enc({json, <<"42">>})),
    ?assertEqual(<<"\"hi\"">>, enc({json, <<"\"hi\"">>})).

iolist_pre_encoded_test() ->
    ?assertEqual(<<"[1,2,3]">>, enc({json, [<<"[">>, "1,2,", <<"3]">>]})),
    ?assertEqual(<<"[1,2,3]">>, enc({json, [$[, [$1, $,, $2], <<",3]">>]})).

inside_array_test() ->
    Pre = {json, <<"{\"x\":42}">>},
    ?assertEqual(<<"[1,{\"x\":42},3]">>, enc([1, Pre, 3])).

inside_object_test() ->
    Pre = {json, <<"[1,2,3]">>},
    ?assertEqual(
        <<"{\"a\":[1,2,3],\"b\":2}">>,
        enc({[{<<"a">>, Pre}, {<<"b">>, 2}]})
    ),
    % Map iteration order is unpredictable so round-trip it
    MapOut = enc(#{<<"a">> => Pre, <<"b">> => 2}),
    ?assertEqual(
        #{<<"a">> => [1, 2, 3], <<"b">> => 2},
        jiffy:decode(MapOut, [return_maps])
    ).

nested_pre_encoded_test() ->
    % Nesting
    Inner = {json, <<"\"raw\"">>},
    EJson = {[
        {<<"k1">>, [1, Inner, 3]},
        {<<"k2">>, {json, <<"null">>}}
    ]},
    ?assertEqual(
        <<"{\"k1\":[1,\"raw\",3],\"k2\":null}">>,
        enc(EJson)
    ).

invalid_non_arity_one_tuple_still_errors_test() ->
    % Other arity-2 things still surface as an error
    ?assertError({invalid_ejson, {foo, bar}}, enc({foo, bar})).

pretty_with_pre_encoded_test() ->
    % No pretty-printing or anything for spliced json
    Pre = {json, <<"[1,2,3]">>},
    Out = iol2b(jiffy:encode([1, Pre], [pretty])),
    ?assertNotEqual(nomatch, binary:match(Out, <<"[1,2,3]">>)).

% Encoder tried to copy a short run of output before each pre-encoded or bignum
% value and keep its buffer around. Test having a bunch of such values before
% and after a yield
many_across_yields_test_() ->
    Opts = [{bytes_per_red, 1}],
    N = 5000,
    Big = 1 bsl 70,
    Keys = [i2b(I) || I <- lists:seq(1, N)],
    [
        {"pre-encoded array",
            ?_assertEqual(
                lists:duplicate(N, 1),
                dec(enc(lists:duplicate(N, {json, <<"1">>}), Opts))
            )},
        {"bignum array",
            ?_assertEqual(
                lists:duplicate(N, Big),
                dec(enc(lists:duplicate(N, Big), Opts))
            )},
        {"pre-encoded object",
            ?_assertEqual(
                {[{K, 1} || K <- Keys]},
                dec(enc({[{K, {json, <<"1">>}} || K <- Keys]}, Opts))
            )}
    ].

% If the run is long enough it will go out with the buffer. Padding make the
% encoder yield. This will likely need to be adjust if we adjust internal
% buffer sizes
pre_encoded_opy_limit_test_() ->
    Pre = {json, <<"0">>},
    Pad = binary:copy(<<"b">>, 4100),
    {timeout, 60, fun() ->
        lists:foreach(fun(N) ->
            Aa = binary:copy(<<"a">>, N),
            ?assertEqual(
                [0, Aa, 0, Aa, 0],
                dec(enc([Pre, Aa, Pre, Aa, Pre]))
            ),
            ?assertEqual(
                [Pad, 0, Aa, 0, Aa, 0],
                dec(enc([Pad, Pre, Aa, Pre, Aa, Pre], [{bytes_per_red, 1}]))
            )
        end, lists:seq(2014, 2074))
    end}.

% An error after a copy, when the encoder still has its buffer
error_after_copy_test_() ->
    Pre = {json, <<"1">>},
    Aa = binary:copy(<<"a">>, 3000),
    LongPres = lists:duplicate(5000, {a, Pre}),
    [
        {"buffer in use",
            ?_assertError(
                {invalid_string, <<255>>},
                enc([Aa, Pre, Pre, <<255>>])
            )},
        {"buffer empty",
            ?_assertError(
                {invalid_object_member, foo},
                enc({[{a, Aa}, {b, Pre}, {c, Pre}, foo]})
            )},
        {"after a yield",
            ?_assertError(
                {invalid_object_member, foo},
                enc({LongPres ++ [foo]}, [{bytes_per_red, 1}])
            )}
    ].

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

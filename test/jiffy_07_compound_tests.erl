% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_07_compound_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


compound_success_test_() ->
    [gen(ok, Case) || Case <- cases(ok)] ++
    [gen(number_key_encoding, Case) || Case <- cases(number_key_encoding)].


compound_failure_test_() ->
    [gen(error, Case) || Case <- cases(error)].


gen(ok, {J, E}) ->
    gen(ok, {J, E, J});
gen(ok, {J1, E, J2}) ->
    {msg("~s", [J1]), [
        {"Decode", ?_assertEqual(E, dec(J1))},
        {"Encode", ?_assertEqual(J2, enc(E))}
    ]};

gen(number_key_encoding, {J, E}) ->
    {msg("~s", [J]), [
        {"Encode", ?_assertEqual(J, enc(E))}
    ]};

gen(error, J) ->
    {msg("Error: ~s", [J]), [
        ?_assertError(_, dec(J))
    ]}.


cases(ok) ->
    [
        {<<"[{}]">>, [{[]}]},
        {<<"{\"foo\":[123]}">>, {[{<<"foo">>, [123]}]}},
        {<<"{\"foo\":{\"bar\":true}}">>,
            {[{<<"foo">>, {[{<<"bar">>, true}]} }]} },
        {<<"{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}">>,
            {[
                {<<"foo">>, []},
                {<<"bar">>, {[{<<"baz">>, true}]}},
                {<<"alice">>, <<"bob">>}
            ]}
        },
        {<<"[-123,\"foo\",{\"bar\":[]},null]">>,
            [
                -123,
                <<"foo">>,
                {[{<<"bar">>, []}]},
                null
            ]
        }
    ];

cases(number_key_encoding) ->
    [
        % Integer keys
        {<<"{\"123\":\"foo\"}">>, {[{123, <<"foo">>}]}},
        {<<"{\"-1\":\"n\"}">>, {[{-1, <<"n">>}]}},
        {<<"{\"123\":\"a\",\"456\":\"b\"}">>, {[{123, <<"a">>}, {456, <<"b">>}]}},
        {<<"{\"123\":\"v\"}">>, #{123 => <<"v">>}},
        % These could change if we swtich away from Ryu
        {<<"{\"1.5\":\"x\"}">>, {[{1.5, <<"x">>}]}},
        {<<"{\"-2.5\":\"x\"}">>, {[{-2.5, <<"x">>}]}},
        {<<"{\"0.0\":\"x\"}">>, {[{0.0, <<"x">>}]}},
        {<<"{\"1.5\":\"a\",\"2.0\":\"b\"}">>, {[{1.5, <<"a">>}, {2.0, <<"b">>}]}},
        {<<"{\"1.5\":\"v\"}">>, #{1.5 => <<"v">>}}
    ];

cases(error) ->
    [
        <<"[{}">>,
        <<"}]">>
    ].

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

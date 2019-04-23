% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_06_object_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


object_success_test_() ->
    [gen(ok, Case) || Case <- cases(ok)].


object_failure_test_() ->
    [gen(error, Case) || Case <- cases(error)].

latin1_atom_test_() ->
    Key = binary_to_atom(<<228>>, latin1), %% `ä`
    Expected = <<"{\"", 195, 164, "\":\"bar\"}">>,
    ?_assertEqual(Expected, enc(#{ Key => <<"bar">> })).

nested_object_segv_test_() ->
    Obj = nested(128),
    Enc = enc(Obj),
    ?_assertEqual(Obj, dec(Enc)).

nested(0) -> <<"bottom">>;
nested(N) -> {[{integer_to_binary(N), nested(N - 1)}]}.

gen(ok, {J, E}) ->
    gen(ok, {J, E, J});
gen(ok, {J1, E, J2}) ->
    {msg("~s", [J1]), [
        {"Decode", ?_assertEqual(E, dec(J1))},
        {"Encode", ?_assertEqual(J2, enc(E))}
    ]};

gen(error, J) ->
    {msg("Error: ~s", [J]), [
        ?_assertThrow({error, _}, dec(J))
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

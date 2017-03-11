-module(jiffy_16_preencode_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


preencode_success_test_() ->
    [gen(ok, Case) || Case <- cases(ok)].


preencode_failure_test_() ->
    [gen(error, Case) || Case <- cases(error)].


gen(ok, {E1, J, E2}) ->
    {msg("~p", [E1]), [
        {"Encode", ?_assertEqual(J, enc(E1))},
        {"Decode", ?_assertEqual(E2, dec(J))}
    ]};

gen(error, E) ->
    {msg("Error: ~p", [E]), [
        ?_assertThrow({error, _}, enc(E))
    ]}.


cases(ok) ->
    TopTests =
        lists:map(
          fun (EJSON) ->
                  JSON = enc(EJSON),
                  {{json, JSON}, JSON, EJSON}
          end, [ 123
               , <<"hello world">>
               , true
               , false
               , {[ {<<"a">>, <<"apple">>}, {<<"b">>, <<"banana">>} ]}
               ]),
    EJSON = [ 1, <<"a">> ],
    JSON = enc(EJSON),
    BuriedTests =
        [ { [ {json, JSON} ], <<"[[1,\"a\"]]">>, [ EJSON ]}
        , { [ 1, {json, JSON}, 3 ], <<"[1,[1,\"a\"],3]">>, [ 1, EJSON, 3 ]}
        , { [ {json, JSON}, {json, JSON} ], <<"[[1,\"a\"],[1,\"a\"]]">>, [ EJSON, EJSON ]}
        , { {[ {<<"a">>, {json, JSON}} ]}, <<"{\"a\":[1,\"a\"]}">>, {[ {<<"a">>, EJSON} ]}}
        ],
    TopTests ++ BuriedTests;

cases(error) ->
    [ {json, true}
    , {json, "true"}
    ].

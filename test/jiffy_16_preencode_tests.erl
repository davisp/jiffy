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

gen(ok, {E, J}) ->
    {msg("~p", [E]), [
        {"Encode", ?_assertEqual(J, enc(E))}
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

    PartialArray = jiffy:partial_encode([ 2, 3 ], []),
    PartialArrayTests =
        [ {[ PartialArray ], <<"[2,3]">>}
        , {[ 1, PartialArray ], <<"[1,2,3]">>}
        , {[ PartialArray, 4 ], <<"[2,3,4]">>}
        , {[ 1, PartialArray, 4 ], <<"[1,2,3,4]">>}
        ],

    PartialObject = jiffy:partial_encode({[ {<<"ii">>, <<"two">>}, {<<"iii">>, 3} ]}, []),
    PartialObjectTests =
        [ {{[ PartialObject ]}, <<"{\"ii\":\"two\",\"iii\":3}">>}
        , {{[ {<<"i">>, 1}, PartialObject ]}, <<"{\"i\":1,\"ii\":\"two\",\"iii\":3}">>}
        , {{[ PartialObject, {<<"iv">>, 4} ]}, <<"{\"ii\":\"two\",\"iii\":3,\"iv\":4}">>}
        , {{[ {<<"i">>, 1}, PartialObject, {<<"iv">>, 4} ]}, <<"{\"i\":1,\"ii\":\"two\",\"iii\":3,\"iv\":4}">>}
        ],

    TopTests ++ BuriedTests ++ PartialArrayTests ++ PartialObjectTests;

cases(error) ->
    [ {json, true}
    , {json, "true"}
    ].

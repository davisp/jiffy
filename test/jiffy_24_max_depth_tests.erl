% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_24_max_depth_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


-define(DOC, <<
    "{\"a\":1,\"b\":{\"c\":[1,2,{\"x\":null}]},"
    "\"d\":[3,{\"e\":4},[]],\"f\":{},\"g\":\"s\"}"
>>).


basic_test_() ->
    Cases = [
        {0, {json, ?DOC}},
        {1, {[
            {<<"a">>, 1},
            {<<"b">>, {json, <<"{\"c\":[1,2,{\"x\":null}]}">>}},
            {<<"d">>, {json, <<"[3,{\"e\":4},[]]">>}},
            {<<"f">>, {json, <<"{}">>}},
            {<<"g">>, <<"s">>}
        ]}},
        {2, {[
            {<<"a">>, 1},
            {<<"b">>, {[{<<"c">>, {json, <<"[1,2,{\"x\":null}]">>}}]}},
            {<<"d">>, [3, {json, <<"{\"e\":4}">>}, {json, <<"[]">>}]},
            {<<"f">>, {[]}},
            {<<"g">>, <<"s">>}
        ]}},
        {3, {[
            {<<"a">>, 1},
            {<<"b">>, {[{<<"c">>, [1, 2, {json, <<"{\"x\":null}">>}]}]}},
            {<<"d">>, [3, {[{<<"e">>, 4}]}, []]},
            {<<"f">>, {[]}},
            {<<"g">>, <<"s">>}
        ]}},
        {4, dec(?DOC)},
        {5, dec(?DOC)},
        {1 bsl 32, dec(?DOC)},
        {(1 bsl 64) - 1, dec(?DOC)}
    ],
    [
        {msg("max_depth ~b", [Depth]),
            ?_assertEqual(Expect, dec(?DOC, [{max_depth, Depth}]))}
     || {Depth, Expect} <- Cases
    ].


% Non-containers are returned as is
terminals_test_() ->
    Cases = [
        {<<"1">>, 1},
        {<<"-1.5">>, -1.5},
        {<<"\"foo\"">>, <<"foo">>},
        {<<"true">>, true},
        {<<"false">>, false},
        {<<"null">>, null},
        {<<"123456789012345678901234567890">>, 123456789012345678901234567890}
    ],
    [
        [
            ?_assertEqual(E, dec(J, [{max_depth, 0}])),
            ?_assertEqual([E, E], dec(<<"[", J/binary, ",", J/binary, "]">>, [{max_depth, 1}])),
            ?_assertEqual({[{<<"k">>, E}]}, dec(<<"{\"k\":", J/binary, "}">>, [{max_depth, 1}]))
        ]
     || {J, E} <- Cases
    ].


empty_containers_test_() ->
    [
        ?_assertEqual({json, <<"[]">>}, dec(<<"[]">>, [{max_depth, 0}])),
        ?_assertEqual({json, <<"{}">>}, dec(<<"{}">>, [{max_depth, 0}])),
        ?_assertEqual([], dec(<<"[]">>, [{max_depth, 1}])),
        ?_assertEqual({[]}, dec(<<"{}">>, [{max_depth, 1}])),
        ?_assertEqual(
            [{json, <<"[]">>}, {json, <<"{}">>}],
            dec(<<"[[],{}]">>, [{max_depth, 1}])
        ),
        ?_assertEqual(
            [{json, <<"[[],{}]">>}, {json, <<"{\"a\":[]}">>}],
            dec(<<"[[[],{}],{\"a\":[]}]">>, [{max_depth, 1}])
        )
    ].


whitespace_test_() ->
    [
        ?_assertEqual(
            {json, <<"[ 1 ,\n2 ]">>},
            dec(<<" \t[ 1 ,\n2 ]\r\n ">>, [{max_depth, 0}])
        ),
        ?_assertEqual(
            {json, <<"{ \"a\" : [ ] }">>},
            dec(<<"\n{ \"a\" : [ ] }\n">>, [{max_depth, 0}])
        ),
        ?_assertEqual(
            {[{<<"a">>, {json, <<"[ 1,\t{ } ]">>}}, {<<"b">>, {json, <<"{\n}">>}}]},
            dec(<<"{ \"a\" :  [ 1,\t{ } ]  , \"b\" : {\n} }">>, [{max_depth, 1}])
        )
    ].


% Brackets, nested quotes and unicode bits shouldn't confuse us
strings_test_() ->
    Cases = [
        <<"[\"]\"]">>,
        <<"[\"[\"]">>,
        <<"{\"}\":\"{\"}">>,
        <<"[\"\\\"]\"]">>,
        <<"[\"\\\\\"]">>,
        <<"{\"a\\\"}\":[\"]\\\\\",\"}\"]}">>,
        <<"[\"\\u005d\",\"\\ud83d\\ude00\"]">>,
        <<"[\"", 16#E2, 16#82, 16#AC, "]\"]">>
    ],
    [
        [
            ?_assertEqual({json, Json}, dec(Json, [{max_depth, 0}])),
            ?_assertEqual(
                [{json, Json}, 1],
                dec(<<"[", Json/binary, ",1]">>, [{max_depth, 1}])
            )
        ]
     || Json <- Cases
    ].


% Round-tripping should work at any depth level
encode_test_() ->
    Exp = enc(dec(?DOC)),
    [?_assertEqual(Exp, enc(dec(?DOC, [{max_depth, D}]))) || D <- lists:seq(0, 5)].


% Decoding the raw after a depth-limited decode should then get is what the
% original would have returned without the raw
expand_test_() ->
    Full = dec(?DOC),
    Maps = dec(?DOC, [return_maps]),
    [
        [
            ?_assertEqual(Full, expand(dec(?DOC, [{max_depth, D}]), [])),
            ?_assertEqual(Maps, expand(dec(?DOC, [return_maps, {max_depth, D}]), [return_maps]))
        ]
     || D <- lists:seq(0, 5)
    ].

return_trailer_test_() ->
    Opts = [return_trailer, {max_depth, 0}],
    [
        ?_assertEqual(
            {has_trailer, {json, <<"[1]">>}, <<"[2]">>},
            dec(<<"[1] [2]">>, Opts)
        ),
        ?_assertEqual(
            {has_trailer, {json, <<"{\"a\":[]}">>}, <<"]">>},
            dec(<<"{\"a\":[]}]">>, Opts)
        ),
        ?_assertEqual(
            {has_trailer, [1, {json, <<"[2]">>}], <<"3">>},
            dec(<<"[1,[2]]3">>, [return_trailer, {max_depth, 1}])
        )
    ].

% Too large floats won'be noticed. They are still valid json
float_range_test_() ->
    [
        ?_assertError({range, <<"1e400">>}, dec(<<"[[1e400]]">>)),
        ?_assertError({range, <<"1e400">>}, dec(<<"[[1e400]]">>, [{max_depth, 2}])),
        ?_assertEqual([{json, <<"[1e400]">>}], dec(<<"[[1e400]]">>, [{max_depth, 1}]))
    ].


% Max depth should fire off the same errors on junk input
error_test_() ->
    Cases = [
        <<"[[1,]]">>,
        <<"[[,]]">>,
        <<"[[,1]]">>,
        <<"[{,}]">>,
        <<"[{\"a\":1,}]">>,
        <<"[[1 2]]">>,
        <<"[[1,2]">>,
        <<"[[1,2">>,
        <<"[[">>,
        <<"[{">>,
        <<"[{\"a\"">>,
        <<"[{\"a\":">>,
        <<"[{\"a\" 1}]">>,
        <<"[{\"a\":}]">>,
        <<"[{\"a\":]}]">>,
        <<"[{\"a\"}]">>,
        <<"[{1:2}]">>,
        <<"[[}]">>,
        <<"[{]]">>,
        <<"[[1]}]">>,
        <<"[{\"a\":1]]">>,
        <<"[[\"a\":1]]">>,
        <<"[[1]]]">>,
        <<"[[n]]">>,
        <<"[[nul]]">>,
        <<"[[tru]]">>,
        <<"[[fals]]">>,
        <<"[[nulll]]">>,
        <<"[[potato]]">>,
        <<"[[01]]">>,
        <<"[[-]]">>,
        <<"[[1.]]">>,
        <<"[[1e]]">>,
        <<"[[.5]]">>,
        <<"[[\"\\u12\"]]">>,
        <<"[[\"\\udc00\"]]">>,
        <<"[[1],[2,]]">>,
        <<"{\"a\":{\"b\":[1,{\"c\":[}]}}">>
    ],
    [
        {msg("~s at max_depth ~b", [hex(Json), Depth]), fun() ->
            {error, Error} = try_dec(Json, []),
            ?assertEqual({error, Error}, try_dec(Json, [{max_depth, Depth}])),
            ?assertEqual(
                {error, Error},
                try_dec(Json, [return_maps, {max_depth, Depth}])
            )
        end}
     || Json <- Cases, Depth <- lists:seq(0, 5)
    ].


bad_option_test_() ->
    [
        ?_assertError(badarg, dec(<<"1">>, [Opt]))
     || Opt <- [
            {max_depth, -42},
            {max_depth, 1.0},
            {max_depth, potato},
            {max_depth, <<"1">>},
            {max_depth, 1 bsl 64},
            {max_depth},
            {max_depth, 1, 2},
            max_depth
        ]
    ].

% Try the stored JSONs
cases_test_() ->
    Files = wildcard("*.json") ++ wildcard("json_test_suite/*.json"),
    % Check that we did the wildcard bits right
    ?assert(length(Files) > 300),
    [
        {tname(File), fun() ->
            [
                ?assert(max_depth_same(fjson(File), Depth, Opts))
             || Depth <- lists:seq(0, 4) ++ [50], Opts <- [[], [return_maps]]
            ]
        end}
     || File <- lists:sort(Files)
    ].

wildcard(Cases) ->
     filelib:wildcard(cases_path(Cases)).

tname(File) ->
    filename:basename(File).

fjson(File) ->
    {ok, Json} = file:read_file(File),
    Json.

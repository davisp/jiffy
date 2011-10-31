#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(83),
    util:test_good(good()),
    util:test_good(uescaped(), [uescape]),
    util:test_errors(errors()),

    test_utf8(utf8_cases()),

    etap:end_tests().

good() ->
    [
        {<<"\"\"">>, <<"">>},
        {<<"\"0\"">>, <<"0">>},
        {<<"\"foo\"">>, <<"foo">>},
        {<<"\"\\\"foobar\\\"\"">>, <<"\"foobar\"">>},
        {<<"\"\\n\\n\\n\"">>, <<"\n\n\n">>},
        {<<"\"\\\" \\b\\f\\r\\n\\t\\\"\"">>, <<"\" \b\f\r\n\t\"">>},
        {<<"\"foo\\u0005bar\"">>, <<"foo", 5, "bar">>},
        {
            <<"\"\\uD834\\uDD1E\"">>,
            <<240, 157, 132, 158>>,
            <<34, 240, 157, 132, 158, 34>>
        }
    ].

uescaped() ->
    [
        {
            <<"\"\\u8CA8\\u5481\\u3002\\u0091\\u0091\"">>,
            <<232,178,168,229,146,129,227,128,130,194,145,194,145>>
        }
    ].

errors() ->
    [
        <<"\"foo">>,
        <<"\"", 0, "\"">>,
        <<"\"\\g\"">>,
        <<"\"\\uFFFF\"">>,
        <<"\"\\uFFFE\"">>,
        <<"\"\\uD834foo\\uDD1E\"">>,
        % CouchDB-345
        <<34,78,69,73,77,69,78,32,70,216,82,82,32,70,65,69,78,33,34>>
    ].

test_utf8([]) ->
    ok;
test_utf8([Case | Rest]) ->
    etap:fun_is(
        fun({error, invalid_string}) -> true; (Else) -> Else end,
        (catch jiffy:encode(Case)),
        lists:flatten(io_lib:format("Invalid utf-8: ~p", [Case]))
    ),
    Case2 = <<34, Case/binary, 34>>,
    etap:fun_is(
        fun({error, {_, invalid_string}}) -> true; (Else) -> Else end,
        (catch jiffy:decode(Case2)),
        lists:flatten(io_lib:format("Invalid utf-8: ~p", [Case2]))
    ),
    test_utf8(Rest).

utf8_cases() ->
    [
        % Stray continuation byte
        <<16#C2, 16#81, 16#80>>,
        <<"foo", 16#80, "bar">>,

        % Invalid Unicode code points
        <<239, 191, 190>>,
        <<237, 160, 129>>,

        % Not enough extension bytes
        <<16#C0>>,

        <<16#E0>>,
        <<16#E0, 16#80>>,

        <<16#F0>>,
        <<16#F0, 16#80>>,
        <<16#F0, 16#80, 16#80>>,

        <<16#F8>>,
        <<16#F8, 16#80>>,
        <<16#F8, 16#80, 16#80>>,
        <<16#F8, 16#80, 16#80, 16#80>>,

        <<16#FC>>,
        <<16#FC, 16#80>>,
        <<16#FC, 16#80, 16#80>>,
        <<16#FC, 16#80, 16#80, 16#80>>,
        <<16#FC, 16#80, 16#80, 16#80, 16#80>>,

        % No data in high bits.
        <<16#C0, 16#80>>,
        <<16#C1, 16#80>>,

        <<16#E0, 16#80, 16#80>>,
        <<16#E0, 16#90, 16#80>>,

        <<16#F0, 16#80, 16#80, 16#80>>,
        <<16#F0, 16#88, 16#80, 16#80>>,

        <<16#F8, 16#80, 16#80, 16#80, 16#80>>,
        <<16#F8, 16#84, 16#80, 16#80, 16#80>>,

        <<16#FC, 16#80, 16#80, 16#80, 16#80, 16#80>>,
        <<16#FC, 16#82, 16#80, 16#80, 16#80, 16#80>>
    ].

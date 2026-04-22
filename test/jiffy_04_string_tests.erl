% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_04_string_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


latin1_atom_test_() ->
    Key = list_to_atom([228]), %% `ä`
    Expected = <<"{\"", 195, 164, "\":\"bar\"}">>,
    ?_assertEqual(Expected, enc({[{Key, <<"bar">>}]})).

% These are slightly sneaky and contain a NUL
latin1_nul_atom_test_() ->
    Bad = binary_to_atom(<<0, 1, 255, 255, 255, 255>>, latin1),
    [
        ?_assertEqual(<<"\"\\u0000\\u0001ÿÿÿÿ\""/utf8>>, enc(Bad)),
        ?_assertEqual(<<"\"\\u0000\\u0001\\u00FF\\u00FF\\u00FF\\u00FF\"">>,
                      enc(Bad, [uescape]))
    ].

% From issue https://github.com/davisp/jiffy/issues/231
% ERL_NIF_UTF8 was added in NIF 2.17 (OTP 26) though
-if(?OTP_RELEASE >= 26).
utf8_atom_test_() ->
    % 2-byte UTF8
    Satas = binary_to_atom(<<"ŝatas"/utf8>>, utf8),
    % 3-byte UTF8 (Google translated this as "Hello")
    Hello = binary_to_atom(<<"你好"/utf8>>, utf8),
    % 4-byte UTF8 (Rocket)
    Rocket = binary_to_atom(<<"🚀"/utf8>>, utf8),
    [
        ?_assertEqual(<<"\"", "ŝatas"/utf8, "\"">>, enc(Satas)),
        ?_assertEqual(<<"\"", "你好"/utf8, "\"">>, enc(Hello)),
        ?_assertEqual(<<"\"", "🚀"/utf8, "\"">>, enc(Rocket)),
        ?_assertEqual(<<"\"\\u015Datas\"">>, enc(Satas, [uescape])),
        ?_assertEqual(<<"\"\\u4F60\\u597D\"">>, enc(Hello, [uescape])),
        ?_assertEqual(<<"\"\\uD83D\\uDE80\"">>,  enc(Rocket, [uescape])),
        ?_assertEqual(<<"{\"", "ŝatas"/utf8, "\":\"v\"}">>, enc(#{Satas => <<"v">>})),
        ?_assertEqual(atom_to_binary(Satas, utf8), dec(enc(Satas))),
        ?_assertEqual(atom_to_binary(Hello, utf8), dec(enc(Hello))),
        ?_assertEqual(atom_to_binary(Rocket, utf8), dec(enc(Rocket)))
    ].
-else.
utf8_atom_test_() ->
    % ERL_NIF_UTF8 isn't available so these atoms can't be extracted.
    Satas = binary_to_atom(<<"ŝatas"/utf8>>, utf8),
    Hello = binary_to_atom(<<"你好"/utf8>>, utf8),
    Rocket = binary_to_atom(<<"🚀"/utf8>>, utf8),
    [
        ?_assertError({invalid_string, _}, enc(Satas)),
        ?_assertError({invalid_string, _}, enc(Hello)),
        ?_assertError({invalid_string, _}, enc(Rocket))
    ].
-endif.

atom_key_test_() ->
    [
        ?_assertEqual(<<"{\"foo\":1}">>, enc({[{foo, 1}]})),
        ?_assertEqual(<<"{\"bar\":2}">>, enc({[{bar, 2}]}))
    ].


string_success_test_() ->
    [gen(ok, Case) || Case <- cases(ok)].


string_uescaped_test_() ->
    [gen(uescaped, Case) || Case <- cases(uescaped)].


string_error_test_() ->
    [gen(error, Case) || Case <- cases(error)].


string_utf8_test_() ->
    [gen(utf8, Case) || Case <- cases(utf8)].


string_bad_utf8_key_test_() ->
    Cases = cases(bad_utf8_key),
    {{J}, {E}} = hd(Cases),
    ExtraProps = [{<<"abcdeefeadasffasdfa">>, I} || I <- lists:seq(1, 10000)],
    Big = {{ExtraProps ++ J}, {ExtraProps ++ E}},
    AllCases = [Big | Cases],
    [gen(bad_utf8_key, Case) || Case <- AllCases].


string_escaped_slashes_test_() ->
    [gen(escaped_slashes, Case) || Case <- cases(escaped_slashes)].

gen(ok, {J, E}) ->
    gen(ok, {J, E, J});
gen(ok, {J1, E, J2}) ->
    {msg("ok - ~s", [J1]), [
        {"Decode", ?_assertEqual(E, dec(J1))},
        {"Encode", ?_assertEqual(J2, enc(E))}
    ]};

gen(uescaped, {J, E}) ->
    {msg("uescape - ~s", [J]), [
        {"Decode", ?_assertEqual(E, dec(J))},
        {"Encode", ?_assertEqual(J, enc(E, [uescape]))}
    ]};

gen(error, J) ->
    {msg("error - ~s", [J]), [
        ?_assertError(_, dec(J))
    ]};

gen(utf8, {Case, Fixed}) ->
    Case2 = <<34, Case/binary, 34>>,
    Fixed2 = <<34, Fixed/binary, 34>>,
    {msg("UTF-8: ~s", [hex(Case)]), [
        ?_assertError({invalid_string, _}, enc(Case)),
        ?_assertEqual(Fixed2, enc(Case, [force_utf8])),
        ?_assertError({_, invalid_string}, dec(Case2))
    ]};

gen(bad_utf8_key, {J, E}) ->
    {msg("Bad UTF-8 key: - ~p", [size(term_to_binary(J))]), [
        ?_assertError({invalid_object_member_key, _}, enc(J)),
        ?_assertEqual(E, dec(enc(J, [force_utf8])))
    ]};

gen(escaped_slashes, {J, E}) ->
    {msg("escaped_slashes - ~s", [J]), [
        {"Decode", ?_assertEqual(E, dec(J))},
        {"Encode", ?_assertEqual(J, enc(E, [escape_forward_slashes]))}
    ]}.

cases(ok) ->
    [
        {<<"\"\"">>, <<"">>},
        {<<"\"/\"">>, <<"/">>},
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
        },
        {<<"\"\\uFFFF\"">>, <<239,191,191>>, <<34,239,191,191,34>>},
        {<<"\"\\uFFFE\"">>, <<239,191,190>>, <<34,239,191,190,34>>}
    ];

cases(uescaped) ->
    [
        {
            <<"\"\\u8CA8\\u5481\\u3002\\u0091\\u0091\"">>,
            <<232,178,168,229,146,129,227,128,130,194,145,194,145>>
        },
        {
            <<"\"\\uD834\\uDD1E\"">>,
            <<240, 157, 132, 158>>
        },
        {
            <<"\"\\uD83D\\uDE0A\"">>,
            <<240, 159, 152, 138>>
        },
        {
            <<"\"\\uDBFF\\uDFFF\"">>,
            <<244, 143, 191, 191>>
        }
    ];

cases(error) ->
    [
        "\"",
        <<"\"foo">>,
        <<"\"", 0, "\"">>,
        <<"\"\\g\"">>,
        <<"\"\\uD834foo\\uDD1E\"">>,
        <<"\"\\u", 200, 200, 200, 200, "\"">>,
        % CouchDB-345
        <<34,78,69,73,77,69,78,32,70,216,82,82,32,70,65,69,78,33,34>>,
        % Lone high surrogate followed by non-backslash
        <<"\"\\uD834a\"">>,
        % Lone high surrogate followed by backslash but not 'u'
        <<"\"\\uD834\\n\"">>,
        % Lone high surrogate followed by \u but not a valid low surrogate
        <<"\"\\uD834\\u0041\"">>,
        % Truncated \uXX (not enough hex digits)
        <<"\"\\u00\"">>,
        % Invalid hex digit in \u escape
        <<"\"\\uZZZZ\"">>,
        % Same story as \uD834\n but with more trailers to pass the length
        % guard and reach the '\u' check for the low surrogate. We're down in
        % the weeds, as it were.
        <<"\"\\uD834\\nabcdef\"">>,
        % \uD834\u<bad hex> low surrogate hex error
        <<"\"\\uD834\\uZZZZ\"">>
    ];

cases(utf8) ->
    [
        % Stray continuation byte
        {<<16#C2, 16#81, 16#80>>, <<16#C2, 16#81, 16#EF, 16#BF, 16#BD>>},
        {<<"foo", 16#80, "bar">>, <<"foo", 16#EF, 16#BF, 16#BD, "bar">>},

        % Not enough extension bytes
        {<<16#C0>>, <<16#EF, 16#BF, 16#BD>>},

        {<<16#E0>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#E0, 16#80>>, <<16#EF, 16#BF, 16#BD>>},

        {<<16#F0>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#F0, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#F0, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},

        {<<16#F8>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#F8, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#F8, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#F8, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},

        {<<16#FC>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#FC, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#FC, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#FC, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#FC, 16#80, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},

        % No data in high bits.
        {<<16#C0, 16#80>>, <<"\\u0000">>},
        {<<16#C1, 16#80>>, <<"@">>},

        {<<16#E0, 16#80, 16#80>>, <<"\\u0000">>},
        {<<16#E0, 16#90, 16#80>>, <<16#D0, 16#80>>},

        {<<16#F0, 16#80, 16#80, 16#80>>, <<"\\u0000">>},
        {<<16#F0, 16#88, 16#80, 16#80>>, <<16#E8, 16#80, 16#80>>},

        % UTF-8-like sequenecs of greater than 4 bytes
        % aren't valid and are replaced with a single
        % replacement 0xFFFD character.
        {<<16#F8, 16#80, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#F8, 16#84, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#FC, 16#80, 16#80, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>},
        {<<16#FC, 16#82, 16#80, 16#80, 16#80, 16#80>>, <<16#EF, 16#BF, 16#BD>>}
    ];

cases(bad_utf8_key) ->
    [
        {
            {[{<<"foo", 16#80, "bar">>, true}]},
            {[{<<"foo", 16#EF, 16#BF, 16#BD, "bar">>, true}]}
        }
    ];

cases(escaped_slashes) ->
    [
        {<<"\"\\/\"">>, <<"/">>},
        {<<"\"foo\\/bar\\/baz\"">>, <<"foo/bar/baz">>}
    ].


atom_escaped_slashes_test_() ->
    [
        ?_assertEqual(<<"\"a\\/b\"">>,
            enc('a/b', [escape_forward_slashes])),
        ?_assertEqual(<<"\"a/b\"">>, enc('a/b')),
        ?_assertEqual(<<"{\"a\\/b\":1}">>,
            enc({[{'a/b', 1}]}, [escape_forward_slashes])),
        ?_assertEqual(<<"\"foo\\/bar\\/baz\\/potato\"">>,
            enc('foo/bar/baz/potato', [escape_forward_slashes]))
    ].

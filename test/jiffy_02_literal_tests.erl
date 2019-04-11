% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_02_literal_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


true_test_() ->
    {"true", [
        {"Decode", ?_assertEqual(true, dec(<<"true">>))},
        {"Encode", ?_assertEqual(<<"true">>, enc(true))}
    ]}.


false_test_() ->
    {"false", [
        {"Decode", ?_assertEqual(false, dec(<<"false">>))},
        {"Encode", ?_assertEqual(<<"false">>, enc(false))}
    ]}.


null_test_() ->
    {"null", [
        {"Decode", ?_assertEqual(null, dec(<<"null">>))},
        {"Encode", ?_assertEqual(<<"null">>, enc(null))}
    ]}.

nil_test_() ->
    {"null", [
        {"Decode", ?_assertEqual(nil, dec(<<"null">>, [use_nil]))},
        {"Encode", ?_assertEqual(<<"null">>, enc(nil, [use_nil]))}
    ]}.

null_term_test_() ->
    T = [
        {undefined, [{null_term, undefined}]},
        {whatever, [{null_term, whatever}]},
        {undefined, [use_nil, {null_term, undefined}]},
        {nil, [{null_term, undefined}, use_nil]},
        {whatever, [{null_term, undefined}, {null_term, whatever}]}
    ],
    {"null_term",
        [?_assertEqual(R, dec(<<"null">>, O)) || {R, O} <- T]}.

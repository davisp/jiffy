% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_02_literal_tests).

-include_lib("eunit/include/eunit.hrl").


true_test_() ->
    {"true", [
        {"Decode", ?_assertEqual(true, jiffy:decode(<<"true">>))},
        {"Encode", ?_assertEqual(<<"true">>, jiffy:encode(true))}
    ]}.


false_test_() ->
    {"false", [
        {"Decode", ?_assertEqual(false, jiffy:decode(<<"false">>))},
        {"Encode", ?_assertEqual(<<"false">>, jiffy:encode(false))}
    ]}.


null_test_() ->
    {"null", [
        {"Decode", ?_assertEqual(null, jiffy:decode(<<"null">>))},
        {"Encode", ?_assertEqual(<<"null">>, jiffy:encode(null))}
    ]}.

nil_test_() ->
    {"null", [
        {"Decode", ?_assertEqual(nil, jiffy:decode(<<"null">>, [use_nil]))},
        {"Encode", ?_assertEqual(<<"null">>, jiffy:encode(nil, [use_nil]))}
    ]}.

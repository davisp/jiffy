% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_16_attempt_atom_tests).

-include_lib("eunit/include/eunit.hrl").

attempt_atom_test_() ->
    Opts = [attempt_atom],
    _ = key_is_atom,
    Cases = [
        {<<"{\"key_no_atom\":1}">>, {[{<<"key_no_atom">>, 1}]}},
        {<<"{\"key_is_atom\":1}">>, {[{key_is_atom, 1}]}}
    ],
    {"Test attempt_atom", lists:map(fun({Data, Result}) ->
        ?_assertEqual(Result, jiffy:decode(Data, Opts))
    end, Cases)}.

-ifndef(JIFFY_NO_MAPS).

attempt_atom_map_test_() ->
    Opts = [attempt_atom, return_maps],
    _ = key_is_atom,
    Cases = [
        {<<"{\"key_no_atom\":1}">>, #{<<"key_no_atom">> => 1}},
        {<<"{\"key_is_atom\":1}">>, #{key_is_atom => 1}}
    ],
    {"Test attempt_atom_map", lists:map(fun({Data, Result}) ->
        ?_assertEqual(Result, jiffy:decode(Data, Opts))
    end, Cases)}.

-endif.

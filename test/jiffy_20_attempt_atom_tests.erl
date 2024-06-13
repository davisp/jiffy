% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_20_attempt_atom_tests).

-include_lib("eunit/include/eunit.hrl").

attempt_atom_test_() ->
    Opts = [{labels, attempt_atom}],
    _ = key_is_atom,
    Cases = [
        {<<"{\"key_no_atom\":1}">>, {[{<<"key_no_atom">>, 1}]}},
        {<<"{\"key_is_atom\":1}">>, {[{key_is_atom, 1}]}}
    ],
    {"Test attempt_atom", lists:map(fun({Data, Result}) ->
        ?_assertEqual(Result, jiffy:decode(Data, Opts))
    end, Cases)}.

attempt_atom_dedupe_keys_test_() ->
    Opts = [{labels, attempt_atom}, dedupe_keys],
    _ = foo,
    Cases = [
       % Basic test
        {
            {[{<<"foo">>, 1}, {<<"foo">>, 2}]},
            {[{foo, 2}]}
        }
    ],
    {"Test _dedupe_keys", lists:map(fun({Data, Result}) ->
        Json = jiffy:encode(Data),
        ?_assertEqual(Result, jiffy:decode(Json, Opts))
    end, Cases)}.

-ifndef(JIFFY_NO_MAPS).

attempt_atom_map_test_() ->
    Opts = [{labels, attempt_atom}, return_maps],
    _ = key_is_atom,
    Cases = [
        {<<"{\"key_no_atom\":1}">>, #{<<"key_no_atom">> => 1}},
        {<<"{\"key_is_atom\":1}">>, #{key_is_atom => 1}}
    ],
    {"Test attempt_atom_map", lists:map(fun({Data, Result}) ->
        ?_assertEqual(Result, jiffy:decode(Data, Opts))
    end, Cases)}.

-endif.

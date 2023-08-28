% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_19_existing_atom_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

existing_atom_test() ->
    Opts = [{labels, existing_atom}],
    Json = {[{key_is_atom, 1}]},
    Data = enc(Json),
    ?_assertMatch(Json, jiffy:decode(Data, Opts)).

existing_atom_no_atom_test() ->
    Opts = [{labels, existing_atom}],
    Json = {[{<<"key_is_no_atom">>, 1}]},
    Data = enc(Json),
    ?_assertException(exit, _, jiffy:decode(Data, Opts)).

existing_atom_dedupe_keys_test_() ->
    Opts = [{labels, existing_atom}, dedupe_keys],
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

existing_atom_map_test() ->
    Opts = [{labels, existing_atom}, return_maps],
    Json = #{key_is_atom => 1},
    Data = enc(Json),
    ?_assertMatch(Json, jiffy:decode(Data, Opts)).

existing_atom_map_no_atom_test() ->
    Opts = [{labels, existing_atom}, return_maps],
    Json = #{<<"key_is_no_atom">> => 1},
    Data = enc(Json),
    ?_assertException(exit, _, jiffy:decode(Data, Opts)).

-endif.

% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_18_atom_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

attempt_atom_test() ->
    Opts = [{labels, atom}],
    K1 = mk_key(),
    K2 = mk_key(),
    Json = enc({[{K1, 1}, {K2, 2}, {foo, 3}]}),
    {Props} = dec(Json, Opts),
    ?_assertEqual(3, length(Props)),
    [?_assertEqual(true, is_atom(K)) || {K, _V} <- Props].

attempt_atom_dedupe_keys_test() ->
    Opts = [{labels, atom}, dedupe_keys],
    K1 = mk_key(),
    Json = enc({[{K1, 1}, {K1, 2}]}),
    {[{K, V}]} = dec(Json, Opts),
    ?_assertEqual(true, is_atom(K)),
    ?_assertEqual(K1, atom_to_binary(K)),
    ?_assertEqual(V, 2).

-ifndef(JIFFY_NO_MAPS).

attempt_atom_map_test() ->
    Opts = [{labels, atom}, return_maps],
    K1 = mk_key(),
    K2 = mk_key(),
    Json = enc({[{K1, 1}, {K2, 2}, {foo, 3}]}),
    Map = dec(Json, Opts),
    ?_assertEqual(3, map_size(Map)),
    maps:map(fun(K, _) -> ?_assertEqual(true, is_atom(K)) end, Map).

-endif.

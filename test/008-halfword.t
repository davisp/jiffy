#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(unknown),

    etap:is(jiffy:decode(<<"1">>) =:= 1, true, "1 =:= 1"),
    etap:is(jiffy:decode(<<"1">>) == 1, true, "1 == 1"),

    etap:end_tests().


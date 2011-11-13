#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),

    Cases = read_cases(),

    etap:plan(length(Cases)),
    lists:foreach(fun(Case) -> test(Case) end, Cases),
    etap:end_tests().

test({Name, Json, {error, _}=Erl}) ->
    etap:is((catch jiffy:decode(Json)), Erl, Name);
test({Name, Json, Erl}) ->
    etap:is(jiffy:decode(Json), Erl, Name).

read_cases() ->
    CasesPath = filename:join(["test", "cases", "*.json"]),
    FileNames = lists:sort(filelib:wildcard(CasesPath)),
    lists:map(fun(F) -> make_pair(F) end, FileNames).

make_pair(FileName) ->
    {ok, Json} = file:read_file(FileName),
    {BaseName, _} = lists:splitwith(fun(C) -> C /= $. end, FileName),
    ErlFname = BaseName ++ ".eterm",
    {ok, [Term]} = file:consult(ErlFname),
    {BaseName, Json, Term}.

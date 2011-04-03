#! /usr/bin/env escript

main([]) ->
    code:add_pathz("test"),
    code:add_pathz("ebin"),
    
    Cases = read_cases(),

    etap:plan(length(Cases)),
    lists:foreach(fun(Case) -> test(Case) end, Cases),
    etap:end_tests().

test({Name, Json, Erl}) ->
    etap:is(jiffy:decode(Json), Erl, Name).

read_cases() ->
    CasesPath = filename:join(["test", "cases", "*.json"]),
    FileNames = lists:sort(filelib:wildcard(CasesPath)),
    lists:map(fun(F) -> make_pair(F) end, FileNames).

make_pair(FileName) ->
    {ok, Json} = file:read_file(FileName),
    {BaseName, _} = lists:splitwith(fun(C) -> C /= $. end, FileName),
    ErlFname = BaseName ++ ".erl",
    {ok, [Term]} = file:consult(ErlFname),
    case Term of
        {error, _} ->
            {BaseName, Json, Term};
        {error, _, _} ->
            {BaseName, Json, Term};
        _ ->
            {BaseName, Json, {ok, Term}}
    end.

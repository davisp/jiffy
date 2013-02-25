#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

filename() -> "test/cases/short-doubles.txt".

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(100000),

    etap:diag("Loading test cases..."),
    {ok, Cases} = file:consult(filename()),

    etap:diag("Running tests..."),
    ok = run_tests(Cases),

    etap:end_tests().


run_tests([]) ->
    ok;
run_tests([Double | Rest]) ->
    RoundTrip = jiffy:decode(jiffy:encode(Double)),
    Desc = lists:flatten(io_lib:format("~e", [Double])),
    etap:is(RoundTrip, Double, "Roundtrip: " ++ Desc),
    run_tests(Rest).


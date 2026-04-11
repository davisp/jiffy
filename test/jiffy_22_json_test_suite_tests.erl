% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.
%
% Tests from https://github.com/nst/JSONTestSuite
%
% y_ : valid JSON, must be accepted
% n_ : invalid JSON, must be rejected
% i_ : implementation-defined, can do whatever

-module(jiffy_22_json_test_suite_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

json_test_suite_test_() ->
    Files = read_files(),
    {Y, N, I} = classify(Files),
    [
        {"y_ (accepts)", [gen_accept(F) || F <- Y]},
        {"n_ (rejects)", [gen_reject(F) || F <- N]},
        {"i_ (whatevs)", [gen_whatever(F) || F <- I]}
    ].


gen_accept({Name, Json}) ->
    {Name, fun() ->
        Dec = jiffy:decode(Json),
        % A few round-trips
        Enc = jiffy:encode(Dec),
        Dec1 = jiffy:decode(Enc),
        ?assertEqual(Dec1, Dec),
        % Can decode with maps
        DecMap = jiffy:decode(Json, [return_maps]),
        EncMap = jiffy:encode(DecMap),
        Dec2 = jiffy:decode(EncMap, [return_maps]),
        ?assertEqual(Dec2, DecMap)
    end}.


gen_reject({Name, Json}) ->
    {Name, fun() ->
        ?assertError({N, _} when is_integer(N), jiffy:decode(Json)),
        ?assertError({N, _} when is_integer(N), jiffy:decode(Json, [return_maps]))
    end}.


% These are implementation. If we accept it let's see if we can
% round trip them at least
%
gen_whatever({Name, Json}) ->
    {Name, fun() ->
        try jiffy:decode(Json) of
            Dec ->
                Enc = jiffy:encode(Dec),
                Dec1 = jiffy:decode(Enc),
                ?assertEqual(Dec1, Dec)
        catch
            error:_ -> ok
        end
    end}.


% Jiffy is linient here and we deviate from strict RFC 8259:
%  - accept truncated exponents e.g. 0.3e+ is 0.3 for us so
%    add an exception for it
%
known_deviations() ->
    [
        "n_number_0.3e+",
        "n_number_0_capital_E+",
        "n_number_0e+",
        "n_number_1.0e+",
        "n_number_1.0e-"
    ].


classify(Files) ->
    classify(Files, [], [], []).

classify([], Y, N, I) ->
    {lists:reverse(Y), lists:reverse(N), lists:reverse(I)};
classify([{Name, _Json} = F | Rest], Y, N, I) ->
    case {Name, lists:member(Name, known_deviations())} of
        {_, true}    -> classify(Rest, Y, N, [F | I]);
        {"y_" ++ _, _} -> classify(Rest, [F | Y], N, I);
        {"n_" ++ _, _} -> classify(Rest, Y, [F | N], I);
        {"i_" ++ _, _} -> classify(Rest, Y, N, [F | I]);
        _              -> classify(Rest, Y, N, I)
    end.


read_files() ->
    Pattern = cases_path("json_test_suite/*.json"),
    FileNames = lists:sort(filelib:wildcard(Pattern)),
    [make_entry(F) || F <- FileNames].


make_entry(FileName) ->
    {ok, Json} = file:read_file(FileName),
    Name = filename:basename(FileName, ".json"),
    {Name, Json}.

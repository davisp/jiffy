% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_19_yielding_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

decode_bytes_per_red_test() ->
    ?assertEqual([1,2,3], dec(<<"[1,2,3]">>, [{bytes_per_red, 50}])).

encode_bytes_per_red_test() ->
    ?assertEqual(<<"[1,2,3]">>, enc([1,2,3], [{bytes_per_red, 50}])).

decode_bytes_per_iter_test() ->
    ?assertEqual([1,2,3], dec(<<"[1,2,3]">>, [{bytes_per_iter, 20000}])).

encode_bytes_per_iter_test() ->
    ?assertEqual(<<"[1,2,3]">>, enc([1,2,3], [{bytes_per_iter, 20000}])).

decode_both_bytes_opts_test() ->
    Opts = [{bytes_per_red, 100}, {bytes_per_iter, 200000}],
    ?assertEqual(1, dec(<<"1">>, Opts)).

encode_both_bytes_opts_test() ->
    Opts = [{bytes_per_red, 100}, {bytes_per_iter, 200000}],
    ?assertEqual(<<"1">>, enc(1, Opts)).

% Give it something large to chew on so it hopefully does some yielding.
% Noticed with coverage enabled we didn't check some yielding paths in CI tests
% otheriwse.
%
decode_large_with_bytes_per_red_test() ->
    Data = iolist_to_binary([
        <<"[">>,
        lists:join(<<",">>, [<<"1">> || _ <- lists:seq(1, 500)]),
        <<"]">>
    ]),
    Result = jiffy:decode(Data, [{bytes_per_red, 20}]),
    ?assertEqual(500, length(Result)).

encode_large_with_bytes_per_red_test() ->
    Data = lists:duplicate(500, 1),
    Encoded = iolist_to_binary(jiffy:encode(Data, [{bytes_per_red, 20}])),
    ?assertEqual(Data, jiffy:decode(Encoded)).

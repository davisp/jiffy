% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_10_short_double_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


filename() ->
    FName = "test/cases/short-doubles.txt",
    case filelib:is_file(FName) of
        true -> FName;
        false -> "../" ++ FName
    end.


short_double_test_() ->
    {ok, Fd} = file:open(filename(), [read, binary, raw]),
    {timeout, 300, ?_assertEqual(0, run(Fd, 0))}.


run(Fd, Acc) ->
    case file:read_line(Fd) of
        {ok, Data} ->
            V1 = re:replace(iolist_to_binary(Data), <<"\.\n">>, <<"">>),
            V2 = iolist_to_binary(V1),
            V3 = <<34, V2/binary, 34>>,
            R = jiffy:encode(jiffy:decode(V3)),
            case R == V3 of
                true -> run(Fd, Acc);
                false -> run(Fd, Acc + 1)
            end;
        eof ->
            Acc
    end.


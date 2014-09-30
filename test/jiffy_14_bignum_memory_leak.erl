% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_14_bignum_memory_leak).

-include_lib("eunit/include/eunit.hrl").


bignum_encoding_leak_test_() ->
    run_gc(),
    Before = erlang:memory(binary),
    encode_bignums(1000000),
    run_gc(),
    After = erlang:memory(binary),
    ?_assert(After - Before < 100000).


run_gc() ->
    [erlang:garbage_collect(Pid) || Pid <- erlang:processes()].


encode_bignums(N) ->
    {_, Ref} = spawn_monitor(fun() ->
        [jiffy:encode(1072502107250210725021072502) || _ <- lists:seq(1, N)]
    end),
    receive
        {'DOWN', Ref, process, _, _} ->
            ok
    end.

% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_18_return_truncated_tests).

-include_lib("eunit/include/eunit.hrl").

cases() ->
    [
        {<<"">>, 1},
        {<<"{">>, 2}
    ].

return_truncated_test_() ->
    Opts = [return_truncated],
    {"Test return_truncated", lists:map(fun({Data, Pos}) ->
        ?_assertEqual({truncated, Pos}, jiffy:decode(Data, Opts))
                                      end, cases())}.

error_truncated_test_() ->
    Opts = [],
    {"Test truncated error case", lists:map(fun({Data, Pos}) ->
        Error = {error, {Pos, truncated_json}},
        ?_assertException(throw, Error, jiffy:decode(Data, Opts))
                                        end, cases())}.

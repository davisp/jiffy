% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_15_trailer_tests).

-include_lib("eunit/include/eunit.hrl").

trailer_test_() ->
   {"trailer", [
        ?_assertEqual(true, jiffy:decode(<<"true">>, [with_trailer]))
    ]}.

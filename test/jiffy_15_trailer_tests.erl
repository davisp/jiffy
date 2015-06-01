% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_15_trailer_tests).

-include_lib("eunit/include/eunit.hrl").

trailer_test_() ->
   Opts = [with_trailer],
   {"trailer", [
        ?_assertEqual(true, jiffy:decode(<<"true">>, Opts)),
        ?_assertMatch({with_trailer, true, <<";">>}, jiffy:decode(<<"true;">>, Opts)),
        ?_assertMatch({with_trailer, true, <<"[]">>}, jiffy:decode(<<"true[]">>, Opts)),
        ?_assertMatch({with_trailer, [], <<"{}">>}, jiffy:decode(<<"[]{}">>, Opts)),
        ?_assertMatch({with_trailer, 1, <<"2 3">>}, jiffy:decode(<<"1 2 3">>, Opts))
    ]}.

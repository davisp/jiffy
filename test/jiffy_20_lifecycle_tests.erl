% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_20_lifecycle_tests).

-include_lib("eunit/include/eunit.hrl").

%% This is more of covarage hacking but we're trying exercise some of the
%% lifecycle callback in jiffy.c

upgrade_test() ->
    % baseline
    ?assertEqual(1, jiffy:decode(<<"1">>)),
    ?assertEqual(<<"1">>, iolist_to_binary(jiffy:encode(1))),

    % soft purge and reload
    code:purge(jiffy),
    {module, jiffy} = code:load_file(jiffy),

    % verify
    ?assertEqual(1, jiffy:decode(<<"1">>)),
    ?assertEqual(<<"1">>, iolist_to_binary(jiffy:encode(1))),

    % another purge should now evict the old module
    code:purge(jiffy),

    % verify
    ?assertEqual(1, jiffy:decode(<<"1">>)),
    ?assertEqual(<<"1">>, iolist_to_binary(jiffy:encode(1))).

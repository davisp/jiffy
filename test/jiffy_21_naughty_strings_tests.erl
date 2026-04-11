% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

% Test the Big List of Naughty Strings (BLNS)
% https://github.com/minimaxir/big-list-of-naughty-strings

-module(jiffy_21_naughty_strings_tests).

-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

naughty_strings_test() ->
    lists:foreach(fun t_string/1, load_strings()).

t_string(String) ->
    ?assertEqual(String, jiffy:decode(jiffy:encode(String))),
    ForceEnc = jiffy:encode(String, [force_utf8]),
    ?assertEqual(String, jiffy:decode(ForceEnc)).

load_strings() ->
    FilePath = cases_path("blns.txt"),
    {ok, Binary} = file:read_file(FilePath),
    Lines = binary:split(Binary, <<"\n">>, [global]),
    % Skip blank lines and comments
    [Line || Line <- Lines, byte_size(Line) > 0, binary:first(Line) =/= $#].

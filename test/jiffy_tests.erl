% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_tests).

-ifdef(JIFFY_DEV).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").


proper_test_() ->
    PropErOpts = [
        {to_file, user},
        {max_size, 15},
        {numtests, 1000}
    ],
    {timeout, 3600, ?_assertEqual([], proper:module(jiffy_tests, PropErOpts))}.


prop_encode_decode() ->
    ?FORALL(Data, json(),
        begin
            %io:format(standard_error, "Data: ~p~n", [Data]),
            Data == jiffy:decode(jiffy:encode(Data))
        end
    ).

prop_encode_decode_pretty() ->
    ?FORALL(Data, json(),
        begin
            Data == jiffy:decode(jiffy:encode(Data, [pretty]))
        end
    ).

prop_encode_not_crash() ->
    ?FORALL(Data, any(), begin catch jiffy:encode(Data), true end).

prop_decode_not_crash_bin() ->
    ?FORALL(Data, binary(), begin catch jiffy:decode(Data), true end).

prop_decode_not_crash_any() ->
    ?FORALL(Data, any(), begin catch jiffy:decode(Data), true end).


% JSON Generation


json_null() ->
    null.


json_boolean() ->
    oneof([true, false]).


json_number() ->
    oneof([integer(), float()]).


json_string() ->
    escaped_utf8_bin().


json_list(S) when S =< 0 ->
    [];
json_list(S) ->
    ?LETSHRINK(
        [ListSize],
        [integer(0, S)],
        vector(ListSize, json_text(S - ListSize))
    ).


json_object(S) when S =< 0 ->
    {[]};
json_object(S) ->
    ?LETSHRINK(
        [ObjectSize],
        [integer(0, S)],
        {vector(ObjectSize, {json_string(), json_text(S - ObjectSize)})}
    ).


json_value() ->
    oneof([
        json_null(),
        json_boolean(),
        json_string(),
        json_number()
    ]).


json_text(S) when S > 0 ->
    ?LAZY(oneof([
        json_list(S),
        json_object(S)
    ]));
json_text(_) ->
    json_value().


json() ->
    ?SIZED(S, json_text(S)).


%% XXX: Add generators
%
% We should add generators that generate JSON binaries directly
% so we can test things that aren't produced by the encoder.
%
% We should also have a version of the JSON generator that inserts
% errors into the JSON that we can test for.


escaped_utf8_bin() ->
    ?SUCHTHAT(Bin,
        ?LET(S, ?SUCHTHAT(L, list(escaped_char()), L /= []),
        unicode:characters_to_binary(S, unicode, utf8)),
        is_binary(Bin)
    ).


escaped_char() ->
    ?LET(C, char(),
        case C of
            $" -> "\\\"";
            C when C == 65534 -> 65533;
            C when C == 65535 -> 65533;
            C when C > 1114111 -> 1114111;
            C -> C
        end
    ).

-endif.

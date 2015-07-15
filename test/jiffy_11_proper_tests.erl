% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_11_proper_tests).

-ifdef(JIFFY_DEV).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").

opts() ->
    [
        {max_size, 15},
        {numtests, 1000}
    ].

run(Name) ->
    {msg("~s", [Name]), [
        {timeout, 300, ?_assert(proper:quickcheck(?MODULE:Name(), opts()))}
    ]}.

proper_encode_decode_test_() ->
    [
        run(prop_enc_dec),
        run(prop_enc_dec_pretty),
        run(prop_dec_trailer),
        run(prop_enc_no_crash),
        run(prop_dec_no_crash_bin),
        run(prop_dec_no_crash_any)
    ].

prop_enc_dec() ->
    ?FORALL(Data, json(),
        begin
            %io:format(standard_error, "Data: ~p~n", [Data]),
            Data == jiffy:decode(jiffy:encode(Data))
        end
    ).

prop_dec_trailer() ->
    ?FORALL({T1, T2}, {json(), json()},
        begin
            B1 = jiffy:encode(T1),
            B2 = jiffy:encode(T2),
            Combiners = [
                <<" ">>,
                <<"\r\t">>,
                <<"\n   \t">>,
                <<"                     ">>
            ],
            lists:foreach(fun(Comb) ->
                Bin = <<B1/binary, Comb/binary, B2/binary>>,
                {has_trailer, T1, Rest} = jiffy:decode(Bin, [return_trailer]),
                T2 = jiffy:decode(Rest)
            end, Combiners),
            true
        end
    ).

-ifndef(JIFFY_NO_MAPS).
to_map_ejson({Props}) ->
    NewProps = [{K, to_map_ejson(V)} || {K, V} <- Props],
    maps:from_list(NewProps);
to_map_ejson(Vals) when is_list(Vals) ->
    [to_map_ejson(V) || V <- Vals];
to_map_ejson(Val) ->
    Val.

prop_map_enc_dec() ->
    ?FORALL(Data, json(),
        begin
            MapData = to_map_ejson(Data),
            MapData == jiffy:decode(jiffy:encode(MapData), [return_maps])
        end
    ).
-endif.

prop_enc_dec_pretty() ->
    ?FORALL(Data, json(),
        begin
            Data == jiffy:decode(jiffy:encode(Data, [pretty]))
        end
    ).

prop_enc_no_crash() ->
    ?FORALL(Data, any(), begin catch jiffy:encode(Data), true end).

prop_dec_no_crash_bin() ->
    ?FORALL(Data, binary(), begin catch jiffy:decode(Data), true end).

prop_dec_no_crash_any() ->
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

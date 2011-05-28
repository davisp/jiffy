% This file is part of Jiffy released under the MIT license. 
% See the LICENSE file for more information.

-module(jiffy).
-export([decode/1, encode/1, encode/2]).
-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

decode(Data) ->
    case nif_decode(Data) of
        {error, _} = Error ->
            throw(Error);
        {partial, EJson} ->
            finish_decode(EJson);
        EJson ->
            EJson
    end.


encode(Data) ->
    encode(Data, []).


encode(Data, Options) ->
    case nif_encode(Data, Options) of
        {error, _} = Error ->
            throw(Error);
        {partial, IOData} ->
            finish_encode(IOData, []);
        IOData ->
            IOData
    end.


finish_decode({bignum, Value}) ->
    list_to_integer(binary_to_list(Value));
finish_decode({bignum_e, Value}) ->
    {IVal, EVal} = case string:to_integer(binary_to_list(Value)) of
        {I, [$e | ExpStr]} ->
            {E, []} = string:to_integer(ExpStr),
            {I, E};
        {I, [$E | ExpStr]} ->
            {E, []} = string:to_integer(ExpStr),
            {I, E}
    end,
    IVal * math:pow(10, EVal);
finish_decode({bigdbl, Value}) ->
    list_to_float(binary_to_list(Value));
finish_decode({Pairs}) when is_list(Pairs) ->
    finish_decode_obj(Pairs, []);
finish_decode(Vals) when is_list(Vals) ->
    finish_decode_arr(Vals, []);
finish_decode(Val) ->
    Val.

finish_decode_obj([], Acc) ->
    {lists:reverse(Acc)};
finish_decode_obj([{K, V} | Pairs], Acc) ->
    finish_decode_obj(Pairs, [{K, finish_decode(V)} | Acc]).

finish_decode_arr([], Acc) ->
    lists:reverse(Acc);
finish_decode_arr([V | Vals], Acc) ->
    finish_decode_arr(Vals, [finish_decode(V) | Acc]).


finish_encode([], Acc) ->
    %% No reverse! The NIF returned us
    %% the pieces in reverse order.
    Acc;
finish_encode([<<_/binary>>=B | Rest], Acc) ->
    finish_encode(Rest, [B | Acc]);
finish_encode([Val | Rest], Acc) when is_integer(Val) ->
    Bin = list_to_binary(integer_to_list(Val)),
    finish_encode(Rest, [Bin | Acc]);
finish_encode(_, _) ->
    throw({error, invalid_ejson}).


init() ->
    SoName = case code:priv_dir(?MODULE) of
        {error, bad_name} ->
            case filelib:is_dir(filename:join(["..", priv])) of
                true ->
                    filename:join(["..", priv, ?MODULE]);
                _ ->
                    filename:join([priv, ?MODULE])
            end;
        Dir ->
            filename:join(Dir, ?MODULE)
    end,
    erlang:load_nif(SoName, 0).


not_loaded(Line) ->
    exit({not_loaded, [{module, ?MODULE}, {line, Line}]}).

nif_decode(_Data) ->
    ?NOT_LOADED.

nif_encode(_Data, _Options) ->
    ?NOT_LOADED.


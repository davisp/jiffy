-module(jiffy).
-export([decode/1, encode/1]).
-define(NOT_LOADED, not_loaded(?LINE)).

-on_load(init/0).

decode(Data) ->
    case nif_decode(Data) of
        {bignum, EJson} ->
            {ok, debignum(EJson)};
        Else ->
            Else
    end.

encode(Data) ->
    nif_encode(Data).


nif_decode(_Data) ->
    ?NOT_LOADED.

nif_encode(_Data) ->
    ?NOT_LOADED.


debignum({bignum, Value}) ->
    list_to_integer(binary_to_list(Value));
debignum({Pairs}) when is_list(Pairs) ->
    debignum_obj(Pairs, []);
debignum(Vals) when is_list(Vals) ->
    debignum_arr(Vals, []);
debignum(Val) ->
    Val.

debignum_obj([], Acc) ->
    {lists:reverse(Acc)};
debignum_obj([{K, V} | Pairs], Acc) ->
    debignum_obj(Pairs, [{K, debignum(V)} | Acc]).

debignum_arr([], Acc) ->
    lists:reverse(Acc);
debignum_arr([V | Vals], Acc) ->
    debignum_arr(Vals, [debignum(V) | Acc]).


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

% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-compile(export_all).
-compile(nowarn_export_all).

iol2b(X) ->
    iolist_to_binary(X).

i2b(X) ->
    integer_to_binary(X).

msg(Fmt, Args) ->
    M1 = io_lib:format(Fmt, Args),
    M2 = re:replace(M1, <<"\r">>, <<"\\\\r">>, [global]),
    M3 = re:replace(M2, <<"\n">>, <<"\\\\n">>, [global]),
    M4 = re:replace(M3, <<"\t">>, <<"\\\\t">>, [global]),
    iol2b(M4).


hex(Bin) when is_binary(Bin) ->
    H1 = [io_lib:format("16#~2.16.0B",[X]) || <<X:8>> <= Bin],
    H2 = string:join(H1, ", "),
    lists:flatten(io_lib:format("<<~s>>", [lists:flatten(H2)])).


dec(V) ->
    jiffy:decode(V).


dec(V, Opts) ->
    jiffy:decode(V, Opts).


enc(V) ->
    iol2b(jiffy:encode(V)).


enc(V, Opts) ->
    iol2b(jiffy:encode(V, Opts)).


try_dec(V, Opts) ->
    try
        {ok, jiffy:decode(V, Opts)}
    catch
        error:Error ->
            {error, Error}
    end.


% Decode the raw JSON left behind by the max_depth decode option
expand({json, Raw}, Opts) when is_binary(Raw) ->
    jiffy:decode(Raw, Opts);
expand({Props}, Opts) when is_list(Props) ->
    {[{K, expand(V, Opts)} || {K, V} <- Props]};
expand(Map, Opts) when is_map(Map) ->
    maps:map(fun(_K, V) -> expand(V, Opts) end, Map);
expand(Vals, Opts) when is_list(Vals) ->
    [expand(V, Opts) || V <- Vals];
expand(Val, _Opts) ->
    Val.


% Decoding with a max_depth has to be the same as decoding without it: either
% the same error, or the same result once the raw parts are decoded too.
max_depth_same(Json, Depth, Opts) ->
    case {try_dec(Json, Opts), try_dec(Json, [{max_depth, Depth} | Opts])} of
        {{ok, Full}, {ok, Limited}} ->
            Full =:= expand(Limited, Opts);
        {{error, {range, _} = Error}, {ok, Limited}} ->
            % Numbers in the raw parts are not converted, so a float which
            % is too large is found only when decoding those
            try expand(Limited, Opts) of
                _ -> false
            catch
                error:Error -> true;
                error:_ -> false
            end;
        {{error, _} = Full, Limited} ->
            Full =:= Limited;
        {_, _} ->
            false
    end.

%% rebar runs eunit with PWD as .eunit/
%% rebar3 runs eunit with PWD as ./
%% this adapts to the differences
cases_path(Suffix) ->
    {ok, Cwd} = file:get_cwd(),
    Prefix = case filename:basename(Cwd) of
        ".eunit" -> "..";
        _ -> "."
    end,
    Path = "test/cases",
    filename:join([Prefix, Path, Suffix]).

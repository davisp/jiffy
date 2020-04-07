% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_18_partials_tests).

-include_lib("eunit/include/eunit.hrl").

decode_levels_test_() ->
    MaxOptMaxLevels = 4,
    {"Test max_levels", lists:map(fun(Json) ->
        [
         begin
             EJson = jiffy:decode(Json, [{max_levels, MaxLevels} | Opts]),
             FullEJson = to_full_json(EJson, MaxLevels, Opts),
             ?_assertEqual(jiffy:decode(Json, Opts), FullEJson)
         end || MaxLevels <- lists:seq(1, MaxOptMaxLevels), Opts <- generate_options_groups()]
    end, jsons())}.

encode_resources_test_() ->
    {"Test encode resources", lists:map(fun(Json) ->
        [
         begin
             EJsonWithResources = jiffy:decode(Json, [{max_levels, 1} | Opts]),
             JsonFromResources = jiffy:encode(EJsonWithResources),
             ?_assertEqual(jiffy:decode(Json, Opts), jiffy:decode(JsonFromResources, Opts))
         end || Opts <- generate_options_groups()]
    end, jsons())}.

encode_partials_test_() ->
    {"Test encode partials", lists:map(fun(Json) ->
        [
         begin
             EJson = jiffy:decode(Json, Opts),
             PartialResource = jiffy:encode(EJson, [partial]),
             true = is_reference(PartialResource),
             PartialIOData = jiffy:encode(PartialResource),
             ?_assertEqual(EJson, jiffy:decode(PartialIOData, Opts))
         end || Opts <- generate_options_groups()]
    end, jsons())}.


jsons() ->
    [
     <<"{\"foo\":\"bar\"}">>,
     <<"{\"foo\":[\"bar\"]}">>,
     <<"[[[[]],\"foo\"], [\"bar\", []], [\"baz\"], [[], 1]]">>,
     <<"{\"foo\":{},\"bar\":{\"baz\":[1,2,3], \"foo2\":{}}}">>
    ].


-ifndef(JIFFY_NO_MAPS).
generate_options_groups() -> generate_options_groups([return_maps]).
-else.
generate_options_groups() -> generate_options_groups([]).
-endif.

generate_options_groups(AvailableOptions) ->
    generate_options_groups(AvailableOptions, [[]]).
generate_options_groups([], Acc) ->
    Acc;
generate_options_groups([Option | AvailableOptions], Acc) ->
    generate_options_groups(AvailableOptions,  [[Option | Group] || Group <- Acc] ++ Acc).


to_full_json(Val, MaxDepth, DecodeOptions) ->
    to_full_json(Val, 0, MaxDepth, DecodeOptions).
to_full_json(_Val, Depth, MaxDepth, _DecodeOptions) when Depth > MaxDepth ->
    error(too_deep);
to_full_json(PartialResource, Depth, MaxDepth, DecodeOptions) when is_reference(PartialResource) ->
    MaxDepth = Depth,
    IOData = jiffy:encode(PartialResource),
    jiffy:decode(IOData, DecodeOptions);
to_full_json({Pairs}, Depth, MaxDepth, DecodeOptions) when is_list(Pairs) ->
    {[{K, to_full_json(V, Depth+1, MaxDepth, DecodeOptions)} || {K, V} <- Pairs]};
to_full_json(Vals, Depth, MaxDepth, DecodeOptions) when is_list(Vals) ->
    [to_full_json(V, Depth+1, MaxDepth, DecodeOptions) || V <- Vals];
to_full_json(Val, Depth, MaxDepth, DecodeOptions) ->
    maybe_map(Val, Depth, MaxDepth, DecodeOptions).

-ifndef(JIFFY_NO_MAPS).
maybe_map(Obj, Depth, MaxDepth, DecodeOptions) when is_map(Obj) ->
    maps:map(fun(_K, V) -> to_full_json(V, Depth+1, MaxDepth, DecodeOptions) end, Obj);
maybe_map(Val, _Depth, _MaxDepth, _DecodeOptions) ->
    Val.
-else.
maybe_map(Val, _Depth, _MaxDepth, _DecodeOptions) ->
    Val.
-endif.


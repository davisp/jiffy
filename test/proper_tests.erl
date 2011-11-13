-module(proper_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([proper_test_/0]).

all() -> proper_ct:testcases(?MODULE).

init_per_testcase(tc_prop_foo, Config) ->
     [{proper, [{numtests, 1000}]} | Config].

%% Helper funs

escaped_char() ->
    ?LET(C, char(),
         case C == $" of
             true  -> "\\\"";
             false -> C
         end).

escaped_utf8_bin() ->
    ?SUCHTHAT(Bin,
              ?LET(S, ?SUCHTHAT(L, list(escaped_char()), L /= []),
                   unicode:characters_to_binary(S, unicode, utf8)),
              is_binary(Bin)).

%% Atomic types
json_null() ->
    null.

json_string() ->
    escaped_utf8_bin().

json_number() ->
    oneof([integer(), float()]).

json_boolean() ->
    oneof([true, false]).

json_atomic() ->
    oneof([json_null(),
           json_string(),
           json_number(),
           json_boolean()]).

%% Compound types
json_object() ->
    ?SIZED(S, json_object(S)).
json_object(S) when S =< 0 ->
    json_atomic();
json_object(S) ->
    frequency([{1, json_object(0)},
               {3, ?LAZY(json_list(S))},
               {3, ?LAZY(
                      ?LETSHRINK(
                         [ObjectSize],
                         [integer(1, S)],
                         ?LETSHRINK(
                            [Object],
                            [{vector(ObjectSize,
                                     {json_string(),
                                      json_object(S - ObjectSize)})}],
                            Object
                           )))}]).

json_list(S) ->
    ?LETSHRINK([ListSize],
               [integer(1, S)],
               vector(ListSize, json_object(S - ListSize))).

json_list() ->
    list(json_object()).

prop_encode_decode() ->
    ?FORALL(Data, json_object(),
            begin
%%                io:format(user, "Data: ~p~n", [Data]),
                Data == jiffy:decode(jiffy:encode(Data))
            end).

prop_encode_not_crash() ->
    ?FORALL(Data, any(),
            begin
                catch jiffy:encode(Data),
                true
            end).

prop_decode_not_crash_bin() ->
    ?FORALL(Data, binary(),
            begin
                catch jiffy:decode(Data),
                true
            end).

prop_decode_not_crash_any() ->
    ?FORALL(Data, any(),
            begin
                catch jiffy:decode(Data),
                true
            end).

proper_test_() ->
    {timeout, 3600,
     ?_assertEqual([], proper:module(proper_tests, [{to_file, user}, {max_size, 60},
                                                    {numtests, 1000}]))}.

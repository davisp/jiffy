-module(proper_tests).
-include_lib("proper/include/proper.hrl").
%%-include_lib("proper_stdlib/include/proper_ct.hrl").
-include_lib("eunit/include/eunit.hrl").
-export([proper_test_/0]).

all() -> proper_ct:testcases(?MODULE).

init_per_testcase(tc_prop_foo, Config) ->
     [{proper, [{numtests, 1000}]} | Config].

-type json_any()    :: json_list()
                     | json_dict()
                     | json_number()
                     | json_string()
                     | json_null().
-type json_list()   :: list(json_any()).
-type json_dict()   :: {[{json_key(), json_any()}]}.
-type json_key()    :: binary().
-type json_number() :: integer() | float().
-type json_string() :: binary().
-type json_null()   :: null.

%% Atomic types
json_null() ->
    null.

json_string() ->
    ?LET(Str, proper_stdgen:utf8_bin(),
         binary:replace(Str, <<"\"">>, <<"\\\"">>, [global])). %good enough

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
json_object(0) ->
    json_atomic();
json_object(S) ->
    frequency([{1, json_object(0)},
               {3, json_list()},
               {9, ?LAZY(
                      ?LETSHRINK(
                         [Node],
                         [{list({json_string(), json_object(S - 1)})}],
                         Node
                        ))}]).

json_list() ->
    list(json_object()).

tree(G) ->
  ?SIZED(S, tree(S, G)).
tree(0, _) ->
    leaf;
tree(S, G) ->
    frequency([
               {1, tree(0, G)},
               {9, ?LAZY(
                      ?LETSHRINK(
                         [L, R],
                         [tree(S div 2, G), tree(S div 2, G)],
                         {node, G, L, R}
                        ))}
              ]).

prop_foo() ->
%%    ?FORALL(Data, json_any(),
%%            Data == jiffy:decode(jiffy:encode(Data))).
    ?FORALL(Data, json_object(),
            begin
                %io:format(user, "Data: ~p~n", [Data]),
                Data == jiffy:decode(jiffy:encode(Data))
            end).

proper_test_() ->
    {timeout, 600,
     ?_assertEqual([], proper:module(proper_tests, [{to_file, user},
                                                    {numtests, 10}]))}.

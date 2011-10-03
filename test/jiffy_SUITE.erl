-module(jiffy_SUITE).
-include_lib("proper/include/proper.hrl").
-include_lib("proper_stdlib/include/proper_ct.hrl").
-compile(export_all).

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

prop_foo() ->
    ?FORALL(Data, json_any(),
            Data == jiffy:decode(jiffy:encode(Data))).

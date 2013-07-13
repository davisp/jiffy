#! /usr/bin/env escript
% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

main([]) ->
    code:add_pathz("ebin"),
    code:add_pathz("test"),

    etap:plan(15),
    util:test_good(good()),
    test_encode(),
    util:test_errors(errors()),
    etap:end_tests().

good() ->
    [
        {<<"{}">>, {[]}},
        {<<"{\"foo\": \"bar\"}">>,
            {[{<<"foo">>, <<"bar">>}]},
            <<"{\"foo\":\"bar\"}">>},
        {<<"\n\n{\"foo\":\r \"bar\",\n \"baz\"\t: 123 }">>,
            {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
            <<"{\"foo\":\"bar\",\"baz\":123}">>}
    ].


test_encode() ->
    % Its ok to use iolist() for keys
    Cases = [
        {<<"{\"foo\":true}">>, {[{"foo",true}]}},
        {<<"{\"foo\":true}">>, {[{["f","o",<<"o">>],true}]}},
        {<<"{\"foo\":[98,97,114]}">>, {[{"foo","bar"}]}}
    ],
    lists:foreach(fun({J, E}) ->
        Msg = lists:flatten(io_lib:format("Encoded ~p", [E])),
        etap:is(jiffy:encode(E), J, Msg)
    end, Cases).


errors() ->
    [
        <<"{">>,
        <<"{,}">>,
        <<"{123:true}">>,
        <<"{false:123}">>,
        <<"{:\"stuff\"}">>,
        <<"{\"key\":}">>,
        <<"{\"key\": 123">>,
        <<"{\"key\": 123 true">>,
        <<"{\"key\": 123,}">>
    ].

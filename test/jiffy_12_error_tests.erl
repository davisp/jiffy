% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_12_error_tests).

-include_lib("eunit/include/eunit.hrl").


-define(ENC_ERROR(Type, Obj, Case),
        ?_assertEqual({error, {Type, Obj}}, (catch jiffy:encode(Case)))).


enc_invalid_ejson_test_() ->
    Type = invalid_ejson,
    Ref = make_ref(),
    {"invalid_ejson", [
        {"the atom 'undefined'", ?ENC_ERROR(Type, undefined, undefined)},
        {"Basic", ?ENC_ERROR(Type, Ref, Ref)},
        {"Nested", ?ENC_ERROR(Type, {Ref, Ref}, {Ref, Ref})}
    ]}.


enc_invalid_string_test_() ->
    Type = invalid_string,
    {"invalid_string", [
        {"Bare strign", ?ENC_ERROR(Type, <<143>>, <<143>>)},
        {"List element", ?ENC_ERROR(Type, <<143>>, [<<143>>])},
        {"Bad obj value", ?ENC_ERROR(Type, <<143>>, {[{foo, <<143>>}]})}
    ]}.

enc_invalid_object_test_() ->
    Type = invalid_object,
    Ref = make_ref(),
    {"invalid_object", [
        {"Number", ?ENC_ERROR(Type, {1}, {1})},
        {"Ref", ?ENC_ERROR(Type, {Ref}, {Ref})},
        {"Tuple", ?ENC_ERROR(Type, {{[]}}, {{[]}})},
        {"Atom", ?ENC_ERROR(Type, {foo}, {foo})}
    ]}.


enc_invalid_object_member_test_() ->
    Type = invalid_object_member,
    {"invalid_object_member", [
        {"Basic", ?ENC_ERROR(Type, foo, {[foo]})},
        {"Basic", ?ENC_ERROR(Type, foo, {[{bar, baz}, foo]})},
        {"Nested", ?ENC_ERROR(Type, foo, {[{bar,{[foo]}}]})},
        {"Nested", ?ENC_ERROR(Type, foo, {[{bar,{[{baz, 1}, foo]}}]})},
        {"In List", ?ENC_ERROR(Type, foo, [{[foo]}])},
        {"In List", ?ENC_ERROR(Type, foo, [{[{bang, true}, foo]}])}
    ]}.


enc_invalid_object_member_arity_test_() ->
    Type = invalid_object_member_arity,
    E1 = {foo},
    E2 = {x, y, z},
    {"invalid_object_member", [
        {"Basic", ?ENC_ERROR(Type, E1, {[E1]})},
        {"Basic", ?ENC_ERROR(Type, E2, {[E2]})},
        {"Basic", ?ENC_ERROR(Type, E1, {[{bar, baz}, E1]})},
        {"Basic", ?ENC_ERROR(Type, E2, {[{bar, baz}, E2]})},
        {"Nested", ?ENC_ERROR(Type, E1, {[{bar,{[E1]}}]})},
        {"Nested", ?ENC_ERROR(Type, E2, {[{bar,{[E2]}}]})},
        {"Nested", ?ENC_ERROR(Type, E1, {[{bar,{[{baz, 1}, E1]}}]})},
        {"Nested", ?ENC_ERROR(Type, E2, {[{bar,{[{baz, 1}, E2]}}]})},
        {"In List", ?ENC_ERROR(Type, E1, [{[E1]}])},
        {"In List", ?ENC_ERROR(Type, E2, [{[E2]}])},
        {"In List", ?ENC_ERROR(Type, E1, [{[{bang, true}, E1]}])},
        {"In List", ?ENC_ERROR(Type, E2, [{[{bang, true}, E2]}])}
    ]}.


enc_invalid_object_member_key_test_() ->
    Type = invalid_object_member_key,
    E1 = {1, true},
    {"invalid_object_member_key", [
        {"Bad string", ?ENC_ERROR(Type, <<143>>, {[{<<143>>, true}]})},
        {"Basic", ?ENC_ERROR(Type, 1, {[{1, true}]})},
        {"Basic", ?ENC_ERROR(Type, [1], {[{[1], true}]})},
        {"Basic", ?ENC_ERROR(Type, {[{foo,bar}]}, {[{{[{foo,bar}]}, true}]})},
        {"Second", ?ENC_ERROR(Type, 1, {[{bar, baz}, E1]})},
        {"Nested", ?ENC_ERROR(Type, 1, {[{bar,{[E1]}}]})},
        {"Nested", ?ENC_ERROR(Type, 1, {[{bar,{[{baz, 1}, E1]}}]})},
        {"In List", ?ENC_ERROR(Type, 1, [{[E1]}])},
        {"In List", ?ENC_ERROR(Type, 1, [{[{bang, true}, E1]}])}
    ]}.

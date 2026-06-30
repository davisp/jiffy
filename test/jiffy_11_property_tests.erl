% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_11_property_tests).

% PropEr is optional used on CI only
-ifdef(WITH_PROPER).


-compile(export_all).
-compile(nowarn_export_all).


-include("jiffy_prop.hrl").
-include("jiffy_util.hrl").


% Keep ?BIN_INC_SIZE in sync with BIN_INC_SIZE in c_src/encoder.c.
-define(BIN_INC_SIZE, 2048).


property_test_() ->
    ?JIFFY_QUICKCHECK(300, 1000).


% Props

prop_enc_dec() ->
    ?FORALL(Data, json(),
        begin
            Data =:= jiffy:decode(jiffy:encode(Data))
        end
    ).


prop_enc_dec_pretty() ->
    ?FORALL(Data, json(),
        begin
            Data =:= jiffy:decode(jiffy:encode(Data, [pretty]))
        end
    ).


prop_dec_trailer() ->
    ?FORALL({T1, Comb, T2}, {json(), combiner(), json()},
        begin
            B1 = iolist_to_binary(jiffy:encode(T1)),
            B2 = iolist_to_binary(jiffy:encode(T2)),
            Bin = <<B1/binary, Comb/binary, B2/binary>>,
            {has_trailer, T1, Rest} = jiffy:decode(Bin, [return_trailer]),
            T2 = jiffy:decode(Rest),
            true
        end
    ).


prop_map_enc_dec() ->
    ?FORALL(Data, json(),
        begin
            MapData = to_map_ejson(Data),
            MapData =:= jiffy:decode(jiffy:encode(MapData), [return_maps])
        end
    ).


prop_enc_no_crash() ->
    ?FORALL(Data, any_term(), begin catch jiffy:encode(Data), true end).


prop_dec_no_crash_any() ->
    ?FORALL(Data, any_term(), begin catch jiffy:decode(Data), true end).


prop_dec_no_crash_bin() ->
    ?FORALL(Data, binary(), begin catch jiffy:decode(Data), true end).


% Go the extra mile to generate larger size strings to exercise values around
% our buffer size limit of 2KB. We want a string also with lots of funky
% escapes, especially shaped like  ...lots of escapes ++ lots of ascii...
prop_enc_buffer_boundary() ->
    ?FORALL({Bin, Opts}, {enc_stress_string(), enc_opts()},
        begin
            Bin =:= jiffy:decode(jiffy:encode(Bin, Opts))
                andalso Bin =:= jiffy:decode(jiffy:encode(Bin, [force_utf8 | Opts]))
         end
).


% Slashes short-cut our fast-forwards to make sure we tests both with and
% without escaping them.
enc_opts() ->
    elements([[], [escape_forward_slashes]]).


enc_stress_string() ->
    ?LET({Prefix, Run}, {enc_escape_prefix(), enc_ascii_run()},
        iolist_to_binary([Prefix, Run])
    ).


enc_escape_prefix() ->
    ?LET({N, C}, {enc_prefix_len(), enc_escape_char()},
        binary:copy(<<C>>, N)
    ).


% With escapes we end up as 1 + 2*N sizes. Choose N to hit more buffer boundaries (2KB)

% that land it in the [2036, 2047] window of the initial 2048-byte buffer.
enc_prefix_len() ->
    frequency([
        {5, choose(1018, 1023)}, % Close to the limit
        {2, choose(1008, 1033)}, % A bit wider interval around the limit
        {2, choose(0, 1200)},    % Small prefixes (this would be a default small int choice)
        {1, choose(2030, 2080)}  % Check over buffer up to 2 buffers worth
    ]).


% Two byte escapes
enc_escape_char() ->
    elements([$\b, $\t, $\n, $\f, $\r, $", $\\]).


% We also need strings made of non-escapes since want to test long runs
% over-buffer of ASCII only character. An escape char will short-cut it.
enc_ascii_run() ->
    ?LET({Len, C}, {enc_run_len(), enc_run_char()},
        binary:copy(<<C>>, Len)
    ).


enc_run_len() ->
    frequency([
        {3, choose(?BIN_INC_SIZE, 5000)},        % small overflow (needs ASan/valgrind)
        {2, choose(8192, 70000)},                % crosses geometric chunk growth
        {2, choose(1 bsl 20, 2 bsl 20)}          % multi-MB: crashes even a plain build
    ]).


% Suchthat filter for ascii only chars
enc_run_char() ->
    ?SUCHTHAT(C, choose($\s, $~), C =/= $" andalso C =/= $\\).


% FORALL_TARGETED is fancy-pants target which uses simulated annealing to
% hill-climb towards some desired maxumum or minimum. We're doing what we did
% above, but just let the test automatically generate string lengths (Ns)
% closer the the desired BIN_INC_SIZE, instead of doing it by hand with
% frequency() + choose(). Let's keep both approaches for now just in case, it
% doesn't hurt the have a belt and suspenders.
prop_enc_boundary_targeted() ->
    Run = binary:copy(<<"a">>, 4 * ?BIN_INC_SIZE),
    ?FORALL_TARGETED(N, integer(0, 8 * ?BIN_INC_SIZE),
        begin
            ?MAXIMIZE((1 + 2 * N) rem ?BIN_INC_SIZE),
            Bin = <<(binary:copy(<<$\b>>, N))/binary, Run/binary>>,
            Bin =:= jiffy:decode(jiffy:encode(Bin))
        end
    ).


to_map_ejson({Props}) ->
    NewProps = [{K, to_map_ejson(V)} || {K, V} <- Props],
    maps:from_list(NewProps);
to_map_ejson(Vals) when is_list(Vals) ->
    [to_map_ejson(V) || V <- Vals];
to_map_ejson(Val) ->
    Val.


% Random any term generation

any_term() ->
    ?SIZED(Size, any_term(Size)).


any_term(0) ->
    any_value();

any_term(Size) ->
    oneof(any_value_types() ++ [
        ?LAZY(any_list(Size)),
        ?LAZY(any_tuple(Size))
    ]).


any_value() ->
    oneof(any_value_types()).


any_value_types() ->
    [
        large_int(),
        integer(),
        float(),
        atom_gen(),
        binary()
    ].


any_list(0) ->
    [];

any_list(Size) ->
    ListSize = Size div 5,
    vector(ListSize, any_term(Size div 2)).


any_tuple(0) ->
    {};

any_tuple(Size) ->
    ?LET(L, any_list(Size), list_to_tuple(L)).


% JSON Generation

json() ->
    ?SIZED(Size, json(Size)).


json(0) ->
    oneof([
        null,
        true,
        false,
        json_number(),
        json_string()
    ]);

json(Size) ->
    frequency([
        {1, null},
        {1, true},
        {1, false},
        {1, json_number()},
        {1, json_string()},
        {5, ?LAZY(json_array(Size))},
        {5, ?LAZY(json_object(Size))}
    ]).


% As of OTP 27, 0.0 =/= -0.0, so exclude -0.0 here (covered explicitly
% elsewhere) to keep exact round-trip matching (which also checks ints stay
% ints).
json_number() ->
    oneof([large_int(), integer(), ?SUCHTHAT(F, float(), F =/= -0.0)]).


json_string() ->
    json_utf8().


json_array(0) ->
    [];

json_array(Size) ->
    ArrSize = Size div 5,
    vector(ArrSize, json(Size div 2)).


json_object(0) ->
    {[]};
json_object(Size) ->
    ObjSize = Size div 5,
    {vector(ObjSize, {json_string(), json(Size div 2)})}.


combiner() ->
    ?SIZED(Size,
        ?LET(L, vector((Size div 4) + 1, oneof([$\r, $\n, $\t, $\s])),
            list_to_binary(L)
        )
    ).

% PropEr unlike EQC doesn't seem to have a large int so we build one here
large_int() ->
    ?LET({I, Shift}, {integer(), choose(0, 96)}, I bsl Shift).


% Atom names (LATIN1)
atom_gen() ->
    ?SIZED(Size,
        ?LET(Cs, vector(Size rem 254, choose(0, 16#FF)), list_to_atom(Cs))
    ).


% Valid UTF-8 binaries
json_utf8() ->
    ?LET(Cps, list(unicode_char()),
        unicode:characters_to_binary(Cps, unicode, utf8)
    ).


unicode_char() ->
    ?SUCHTHAT(C,
        frequency([
            {5, choose(16#20, 16#7E)},        % ASCII
            {2, choose(16#00, 16#1F)},        % Control chars
            {2, choose(16#80, 16#7FF)},       % 2-byte
            {2, choose(16#800, 16#FFFF)},     % 3-byte
            {1, choose(16#10000, 16#10FFFF)}  % 4-byte
        ]),
        C < 16#D800 orelse C > 16#DFFF
    ).


-endif.

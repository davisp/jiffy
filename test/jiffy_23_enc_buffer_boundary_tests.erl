% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.

-module(jiffy_23_enc_buffer_boundary_tests).


-include_lib("eunit/include/eunit.hrl").
-include("jiffy_util.hrl").


% Specifically test large string around our buffer size of 2KB. We'd like the string
% to have lot of escapes but also runs of plain ASCII.

roundtrip(Bin) ->
    ?assertEqual(Bin, dec(enc(Bin))),
    ?assertEqual(Bin, dec(enc(Bin, [force_utf8]))).

lots_of_escapes_followed_by_ascii_test() ->
    Bin = <<(binary:copy(<<$\b>>, 1018))/binary, (binary:copy(<<"a">>, 4096))/binary>>,
    roundtrip(Bin).

% Test around the 2KB boundary
boundary_sweep_2k_test_() ->
    Run = binary:copy(<<"a">>, 4100),
    {timeout, 60, fun() ->
        lists:foreach(fun(N) ->
            Bin = <<(binary:copy(<<$\b>>, N))/binary, Run/binary>>,
            roundtrip(Bin)
        end, lists:seq(990, 1060))
    end}.

% Test around the doubling growth (we grow our buffer to MAX_CHUNK_SIZE = 64KB)
boundary_sweep_chunk_growth_test_() ->
    Run = binary:copy(<<"a">>, 70000),
    {timeout, 120, fun() ->
        lists:foreach(fun(N) ->
            Bin = <<(binary:copy(<<$\b>>, N))/binary, Run/binary>>,
            roundtrip(Bin)
        end, lists:seq(64450, 64500))
    end}.

% A test for escapes + forward slashes
forward_slash_test_() ->
    Run = binary:copy(<<"https://example.com/a/b/">>, 4096),
    {timeout, 30, fun() ->
        lists:foreach(fun(N) ->
            Bin = <<(binary:copy(<<$\b>>, N))/binary, Run/binary>>,
            roundtrip(Bin)
        end, lists:seq(1015, 1025))
    end}.

% A test for escapae + forward slashes with forward slash escape option
escape_forward_slashes_test_() ->
    Run = binary:copy(<<"a">>, 4100),
    {timeout, 30, fun() ->
        lists:foreach(fun(N) ->
            Bin = <<(binary:copy(<<$\b>>, N))/binary, Run/binary>>,
            ?assertEqual(Bin, dec(enc(Bin, [escape_forward_slashes])))
        end, lists:seq(1015, 1025))
    end}.

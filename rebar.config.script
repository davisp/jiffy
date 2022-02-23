% This file is part of Jiffy released under the MIT license.
% See the LICENSE file for more information.
%
% Only run the EQC checks when EQC is present.

HaveEQC = code:which(eqc) =/= non_existing,

ErlOpts = if not HaveEQC -> []; true ->
    [{d, 'HAVE_EQC'}]
end,

Config1 = case lists:keyfind(erl_opts, 1, CONFIG) of
    {erl_opts, Opts} ->
        NewOpts = {erl_opts, Opts ++ ErlOpts},
        lists:keyreplace(erl_opts, 1, CONFIG, NewOpts);
    false ->
        CONFIG ++ [{erl_opts, ErlOpts}]
end,

Config2 = case os:type() of
    {unix, _} ->
        CC = case os:getenv("CC") of
            false -> "cc";
            Else -> Else
        end,
        FLTO_CHECK = "echo 'int main(int argc, char *argv[]) {return 0;}' | "
                ++ CC ++ " -c -x c -o /dev/null -flto -",
        case os:cmd(FLTO_CHECK) of
            [] ->
                {port_env, PortEnv} = lists:keyfind(port_env, 1, Config1),
                NewFlag = {".*", "FLTO_FLAG", "-flto"},
                NewPortEnv = lists:keyreplace("FLTO_FLAG", 2, PortEnv, NewFlag),
                lists:keyreplace(port_env, 1, Config1, {port_env, NewPortEnv});
            _ ->
                Config1
        end;
    _ ->
        Config1
end,

IsRebar2 = case lists:keyfind(rebar, 1, application:loaded_applications()) of
    {rebar, _Desc, Vsn} ->
        case string:split(Vsn, ".") of
            ["2" | _] -> true;
            _ -> false
        end;
    false ->
        false
end,

case IsRebar2 of
    true ->
        Config2;
    false ->
        Config2 ++ [
            {plugins, [{pc, "~> 1.0"}]},
            {artifacts, ["priv/jiffy.so"]},
            {provider_hooks, [
                {post, [
                    {compile, {pc, compile}},
                    {clean, {pc, clean}}
                ]}
            ]}
        ]
end.

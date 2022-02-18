#!/usr/bin/env python3

import os
import shutil
import subprocess as sp

TEST_DIR = ".jiffy-compile-check"

ERLANG_VMS = {
    15: "R15B03-1",
    16: "R16B03-1",
    17: "17.5.6.10",
    18: "18.3.4.11",
    19: "19.3.6.13",
    20: "20.3.8.26",
    21: "21.3.8.24",
    22: "22.3.4.24",
    23: "23.3.4.11",
    24: "24.2.1",
    25: "25.0-rc1"
}

MAX_ERLANG = max(ERLANG_VMS.keys())

ERLANG_COMPATIBILITY = [
    (15, 24),
    (22, MAX_ERLANG)
]

# The rebar3 compatibility matrix was created using the
# ranges in this blog post:
#   https://ferd.ca/you-ve-got-to-upgrade-rebar3.html
REBAR_MATRIX = [
    ("2.6.4", 15, MAX_ERLANG),
    ("3.7.0", 17, 21),
    ("3.13.2", 18, 22),
    ("3.15.2", 19, 23),
    ("3.16.0", 22, 24),
    ("3.18.0", 24, MAX_ERLANG)
]

JTAPP2_URL = "https://github.com/davisp/jtapp2"
REBAR2_URL = "https://github.com/rebar/rebar"
REBAR3_URL = "https://github.com/erlang/rebar3"


def init_test_dir():
    if not os.path.exists(TEST_DIR):
        os.mkdir(TEST_DIR)
    os.chdir(TEST_DIR)
    if not os.path.exists("jiffy"):
        sp.check_call("git clone .. jiffy", shell=True)
    if not os.path.exists("jtapp2"):
        sp.check_call("git clone {}".format(JTAPP2_URL), shell=True)
    if not os.path.exists("rebar"):
        sp.check_call("git clone {}".format(REBAR2_URL), shell=True)
    if not os.path.exists("rebar3"):
        sp.check_call("git clone {}".format(REBAR3_URL), shell=True)


def gen_tests():
    # Check each rebar version on its last supported VM
    for (vsn, min_erl, max_erl) in REBAR_MATRIX:
        yield(vsn, ERLANG_VMS[max_erl], ERLANG_VMS[max_erl])

    # Mixed VM tests
    for (min_compat, max_compat) in ERLANG_COMPATIBILITY:
        for (vsn, min_erl, max_erl) in REBAR_MATRIX:
            if max_erl < min_compat:
                continue
            if min_erl > max_compat:
                continue
            build_vm = max(min_compat, min_erl)
            test_vm = min(max_compat, max_erl)
            yield (vsn, ERLANG_VMS[build_vm], ERLANG_VMS[test_vm])


def run_test(rebar_vsn, build_vm, test_vm):
    msg = "Checking rebar {} built with {} to compile on {}"
    print(msg.format(rebar_vsn, build_vm, test_vm))

    if rebar_vsn[:2] == "2.":
        rebar_dir = "rebar"
    else:
        rebar_dir = "rebar3"
    rebar_path = "../{}/{}".format(rebar_dir, rebar_dir)

    cmds = [
        "cd {} && git checkout -q {}".format(rebar_dir, rebar_vsn),
        "cd {} && git clean -ffxd".format(rebar_dir),
        "cd {} && asdf local erlang {}".format(rebar_dir, build_vm),
        "cd {} && ./bootstrap".format(rebar_dir),
        "cd jiffy && git clean -ffxd",
        "cd jiffy && asdf local erlang {}".format(test_vm),
        "cd jiffy && {} get-deps".format(rebar_path),
        "cd jiffy && {} compile".format(rebar_path),
        "cd jiffy && {} eunit".format(rebar_path),
        "cd jtapp2 && git clean -ffxd",
        "cd jtapp2 && asdf local erlang {}".format(test_vm),
        "cd jtapp2 && {} get-deps".format(rebar_path),
        "cd jtapp2 && {} compile".format(rebar_path),
        "cd jtapp2 && {} eunit".format(rebar_path)
    ]
    for cmd in cmds:
        sp.check_output(cmd, shell=True)

    cmd = "cd jiffy && {} --version".format(rebar_path)
    vsn = sp.check_output(cmd, shell=True).split()
    assert vsn[1].decode("utf-8") == rebar_vsn

    if rebar_vsn.startswith("2."):
        assert build_vm.startswith(vsn[2].decode("utf-8"))
    else:
        # For some reason rebar3 switched to reporting the VM
        # that is executing rebar3, not the version that was
        # used to compile it.
        assert test_vm.startswith(vsn[4].decode("utf-8"))

    cmd = "erl -noshell -eval 'io:fwrite(erlang:system_info(otp_release)), halt().'"
    otp = sp.check_output("cd jiffy && {}".format(cmd), shell=True).decode("utf-8")
    assert test_vm.startswith(otp)


def main():
    init_test_dir()
    for (rebar_vsn, build_vm, test_vm) in gen_tests():
        run_test(rebar_vsn, build_vm, test_vm)


if __name__ == "__main__":
    main()

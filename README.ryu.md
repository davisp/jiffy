# Ryu

Our version of Ryu cames as-is from Erlang/OTP, accepting its format. We're an
Erlang library so we'll accept what Erlang/OTP team goes with.

Erlang's Ryu in turn is a modified version of
[Ryu](https://github.com/ulfjack/ryu), keeping only what's needed, and then
modified to produce Erlang-like formatted floats. For example, original Ryu
will produce `"1E1"` from `10.0`, while Erlang and Jiffy will write `"10.0"`.

# How to update

```sh
OTP_SRC=/path/to/otp

cp $OTP_SRC/erts/emulator/ryu/common.h         c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/d2s.c            c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/d2s_full_table.h c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/d2s_intrinsics.h c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/digit_table.h    c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/ryu.h            c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/to_chars.h       c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/LICENSE-Apache2   c_src/ryu/
cp $OTP_SRC/erts/emulator/ryu/LICENSE-Boost     c_src/ryu/
```

Note: we don't modify the ryu files after this. But we do handle the `-0.0` ==
`0.0` case in `c_src/double.c`.

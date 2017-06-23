defmodule Jiffy do
  @moduledoc """
  A JSON parser as a NIF.

  # Data Format

    | Elixir                       | -> JSON          | -> Elixir  |
    | ---------------------------- | ---------------- | ------- |
    | `nil`                        | `null`           | `nil`   |
    | `true`                       | `true`           | `true`  |
    | `false`                      | `false`          | `false` |
    | `'hi'`                       | `[104, 105]`     | `[104, 105]` |
    | `"hi"`                       | `"hi"`           | `"hi"` |
    | `:hi`                        | `"hi"`           | `"hi"` |
    | `1`                          | `1`              | `1` |
    | `1.25`                       | `1.25`           | `1.25` |
    | `[]`                         | `[]`             | `[]` |
    | `[true, 1.0]`                | `[true, 1.0]`    | `[true, 1.0]` |
    | `%{"foo" => "bar"}`          | `{"foo": "bar"}` | `%{"foo" => "bar"}` |
    | `%{foo: "bar"}`              | `{"foo": "bar"}` | `%{"foo" => "bar"}` |
  """
  @encode_options [:use_nil]
  @decode_options [:use_nil, :return_maps]

  @doc """
  Encode a value to JSON.

  # Unsupported structures

    * Encoding Keywords currently is not supported.
    * Encoding DateTime, Date or other Date-related Elixir structures will return
      `{:error, {:invalid_ejson, any}}`. If you want to encode them - you need to cast
      them to string before encoding.

  # Options

    * `:uescape` - Escapes UTF-8 sequences to produce a 7-bit clean output.
    * `:pretty` - Produce JSON using two-space indentation.
    * `:force_utf8` - Force strings to encode as UTF-8 by fixing broken
      surrogate pairs and/or using the replacement character to remove
      broken UTF-8 sequences in data.
    * `:escape_forward_slashes` - Escapes the `/` character which can be
      useful when encoding URLs in some cases.
    * `{:bytes_per_red, n}` - Refer to the `decode/2` options.
    * `{:bytes_per_iter, n}` - Refer to the `decode/2` options.

  # Examples

      iex> Jiffy.encode([1, 2, 3])
      {:ok, "[1,2,3]"}
  """
  @spec encode(any, opts :: :jiffy.encode_option()) :: {:ok, any()} | {:error, any()}
  def encode(data, opts \\ []) do
    {:ok, encode!(data, opts)}
  catch
    {:error, reason} -> {:error, reason}
  end

  @doc """
  Encode a value to JSON, raises an exception on error.

  For list of options see `encode/2`.

  # Examples

      iex> Jiffy.encode!([1, 2, 3])
      "[1,2,3]"
  """
  @spec encode!(any, opts :: :jiffy.encode_option()) :: {:ok, any()} | no_return()
  def encode!(data, opts \\ []) do
    :jiffy.encode(data, @encode_options ++ opts)
  end

  @doc """
  Decode JSON to a value.

  # Options

    * `:return_trailer` - If any non-whitespace is found after the first
      JSON term is decoded the return value of decode/2 becomes
      `{:has_trailer, first_term, rest_iodata}`. This is useful to
      decode multiple terms in a single binary.
    * `{:bytes_per_red, n}` where `n` &gt;= 0 - This controls the number of
      bytes that Jiffy will process as an equivalent to a reduction. Each
      20 reductions we consume 1% of our allocated time slice for the current
      process. When the Erlang VM indicates we need to return from the NIF.
    * `{:bytes_per_iter, n}` where `n` &gt;= 0 - Backwards compatible option
      that is converted into the `bytes_per_red` value.

  # Examples

      iex> Jiffy.decode("[1,2,3]")
      {:ok, [1, 2, 3]}
  """
  @spec decode(String.t, opts :: :jiffy.decode_option()) :: {:ok, any()} | {:error, atom()}
  def decode(data, opts \\ []) do
    {:ok, decode!(data, opts)}
  catch
    {:error, reason} -> {:error, reason}
  end

  @doc """
  Decode JSON to a value, raises an exception on error.

  For list of options see `decode/2`.

  # Examples

      iex> Jiffy.decode!([1, 2, 3])
      "[1,2,3]"
  """
  @spec decode!(String.t, opts :: :jiffy.decode_option()) :: any() | no_return()
  def decode!(data, opts \\ []) do
    :jiffy.decode(data, @decode_options ++ opts)
  end
end

defmodule Jiffy.Mixfile do
  use Mix.Project

  @version "0.14.11"

  def project do
    [app: :jiffy,
     description: "A JSON parser as a NIF.",
     package: package(),
     version: @version,
     compilers: [:elixir_make] ++ Mix.compilers(),
     elixir: "~> 1.4",
     build_embedded: Mix.env == :prod,
     start_permanent: Mix.env == :prod,
     deps: deps(),
     docs: [source_ref: "v#\{@version\}", main: "readme", extras: ["README.md"]],
     aliases: aliases()]
  end

  def application do
    [extra_applications: [:logger]]
  end

  defp package do
    [contributors: ["davisp"],
     maintainers: ["davisp"],
     licenses: ["MIT", "BSD"],
     links: %{github: "https://github.com/davisp/jiffy"},
     files: ~w(c_src src lib LICENSE mix.exs README.md)]
  end

  defp deps do
    [{:elixir_make, "~> 0.4", runtime: false}]
  end

  defp aliases do
    [clean: ["clean", "clean.make"]]
  end
end

defmodule MiniForth do
  @moduledoc """
  synopsis:
    Compile the .fth file
  usage:
    $ mini_forth example/loop.fth
  options:
    --debug     Print debug info.
  """
  alias MiniForth.{C}

  def main([]) do
    IO.puts(@moduledoc)
  end

  def main([help_opt]) when help_opt == "-h" or help_opt == "--help" do
    IO.puts(@moduledoc)
  end

  def main(args) do
    {opts, positional_args, errors} =
      args
      |> parse_args

    case errors do
      [] ->
        process_args(opts, positional_args)

      _ ->
        IO.puts("Bad option:")
        IO.inspect(errors)
        IO.puts(@moduledoc)
    end
  end

  defp parse_args(args) do
    {opts, cmd_and_args, errors} =
      args
      |> OptionParser.parse(strict: [debug: :boolean])

    {opts, cmd_and_args, errors}
  end

  defp process_args(opts, args) do
    debug = Keyword.has_key?(opts, :debug)

    Application.put_env(:elixir, :mini_forth_debug, debug)

    Enum.with_index(args)
    |> Enum.each(fn {arg, _idx} ->
      # printfn.("#{idx}. #{arg}")

      try_to_run_sv_code(arg)
    end)
  end

  defp concat(x, y), do: x <> "\n" <> y

  defp prelude() do
    {File.read!("core/core.fth"), File.read!("core/core_ext.fth")}
  end

  def run_code(code) do
    {core, core_ext} = prelude()

    code
    |> concat(core)
    |> concat(core_ext)
    |> full_compile()
    |> Map.get(:main)
    |> :interpreter.eval()
  end

  defp full_compile(code) do
    code
    |> C.parse()
    |> C.replace()
    |> C.compile()
  end

  defp try_to_run_sv_code(arg) do
    {core, core_ext} = prelude()
    code = concat(core_ext, File.read!(arg))

    raw =
      concat(core, code)
      |> full_compile()

    raw_without_core =
      code
      |> full_compile()

    main =
      raw
      |> Map.get(:main)

    if main do
      IO.puts("")

      IO.puts(
        "[RAW SCRIPT]\n" <>
          inspect(C.to_asm_string(raw_without_core.main), limit: :infinity) <> "\n"
      )

      main
      |> :interpreter.eval()
      |> print_stacks()
    end

    test =
      raw
      |> Map.get(:test)

    if test do
      case test |> :interpreter.eval() do
        :ok -> IO.puts("Test failed.")
        _X -> IO.puts("Test passed.")
      end
    end
  end

  defp print_stacks({m, a}) do
    IO.puts("[EVAL RESULT]")
    IO.puts("MainStack: " <> inspect(Enum.reverse(m)))
    IO.puts("AltStack:  " <> inspect(Enum.reverse(a)))
    IO.puts("")
  end
end

if Mix.env() == :dev do
  MiniForth.main(System.argv())
end

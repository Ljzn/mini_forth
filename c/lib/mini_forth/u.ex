defmodule MiniForth.U do
  @moduledoc """
  Utils.
  """
  alias MiniForth.P

  def debug(msg, opts) do
    if Application.get_env(:elixir, :mini_forth_debug, false) do
      IO.inspect(msg, opts)
    end

    msg
  end

  def print_stack(main, alt) do
    IO.puts("")
    IO.puts("[Alt Stack]: " <> inspect(Enum.reverse(alt)))
    IO.puts("[MainStack]: " <> inspect(Enum.reverse(main)))
    IO.puts("")
  end
end

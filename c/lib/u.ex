defmodule U do
  @moduledoc """
  Utils.
  """

  def debug(msg, opts) do
    if Application.get_env(:elixir, :mini_forth_debug, false) do
      IO.inspect(msg, opts)
    end

    msg
  end
end

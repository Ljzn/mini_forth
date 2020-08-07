defmodule C do
  @moduledoc """
  Documentation for `C`.
  """

  @doc """
  Hello world.

  ## Examples

      iex> C.hello()
      :world

  """
  def hello do
    :world
  end

  @doc """
  Parse the string code into a map of definations.

  ## Examples

      iex> code = ": add1 1 add ; : add2 add1 add1 ;"
      ...> C.parse(code)
      %{add1: [1, :add], add2: [:add1, :add1]}

  """
  def parse(str) do
    {:ok, result, _, _, _, _} = P.simple_forth(str)

    for {k, v} <- result, into: %{} do
      {k, v}
    end
  end

  @doc """
  Replace the user defined keywrod.
  The max round is 100.

  ## Examples

      iex> C.replace(%{add1: [1, :add], add2: [:add1, :add1]})
      %{add1: [1, :add], add2: [1, :add, 1, :add]}

  """
  def replace(map) do
    do_replace(map, 0)
  end

  defp do_replace(_map, 100), do: raise("reach max replace limit")

  defp do_replace(map, t) do
    map1 =
      Enum.reduce(map, map, fn {k, v}, acc ->
        Enum.reduce(acc, %{}, fn {k0, v0}, acc1 ->
          Map.put(acc1, k0, update_sentance(v0, k, v))
        end)
      end)

    if map1 == map do
      map1
    else
      do_replace(map1, t + 1)
    end
  end

  defp update_sentance(s, k, v) do
    for w <- s do
      if w == k do
        v
      else
        w
      end
    end
    |> List.flatten()
  end
end

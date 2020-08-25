defmodule MiniForth.C do
  @moduledoc """
  C stands for compiler.
  """
  alias MiniForth.{P, U}

  @doc """
  Parse the string code into a map of definations.

  ## Examples

      iex> code = ": add1 1 add ; : add2 add1 add1 ;"
      ...> C.parse(code)
      %{add1: [1, :add], add2: [:add1, :add1]}

  """
  def parse(str) do
    # delete \ comments
    str =
      str
      |> String.split("\n")
      |> Enum.reject(fn x ->
        x = String.trim_leading(x)
        String.starts_with?(x, "\\")
      end)
      |> Enum.join("\n")

    {:ok, result, _, _, _, _} =
      P.simple_forth(str)
      |> U.debug(lable: "after parse")

    for {k, v} <- result, into: %{} do
      {k, v}
    end
    |> U.debug(label: "parsed", limit: :infinity)
  end

  @doc """
  Replace the user defined keywrod.
  The max round is 1000.

  ## Examples

      iex> C.replace(%{add1: [1, :add], add2: [:add1, :add1]})
      %{add1: [1, :add], add2: [1, :add, 1, :add]}

  """
  def replace(map) do
    do_replace(map, 0)
    |> U.debug(label: "replaced", limit: :infinity)
  end

  defp do_replace(_map, 1000), do: raise("reach max replace limit")

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

  def to_asm_string(list) do
    Enum.map(list, &do_to_asm_string/1)
    |> Enum.join(" ")
  end

  defp do_to_asm_string(""), do: "OP_0"
  defp do_to_asm_string(:+), do: "OP_ADD"
  defp do_to_asm_string(:-), do: "OP_SUB"
  defp do_to_asm_string(:*), do: "OP_MUL"
  defp do_to_asm_string(:/), do: "OP_DIV"
  defp do_to_asm_string(:%), do: "OP_MOD"
  defp do_to_asm_string(:=), do: "OP_EQUAL"
  defp do_to_asm_string(:"1-"), do: "OP_1SUB"
  defp do_to_asm_string(:fas), do: "OP_FROMALTSTACK"
  defp do_to_asm_string(:tas), do: "OP_TOALTSTACK"
  defp do_to_asm_string(:"=verify"), do: "OP_EQUALVERIFY"
  defp do_to_asm_string(:"num=verify"), do: "OP_NUMEQUALVERIFY"

  defp do_to_asm_string(atom) when is_atom(atom),
    do: "OP_" <> (to_string(atom) |> String.upcase())

  defp do_to_asm_string(-1), do: "OP_1NEGATE"
  defp do_to_asm_string(x) when x in 0..16, do: "OP_#{x}"

  defp do_to_asm_string(x) when is_integer(x),
    do: x |> :interpreter.num2bin() |> Base.encode16(case: :lower)

  defp do_to_asm_string(x) when is_binary(x), do: Base.encode16(x, case: :lower)

  def compile(map) do
    for {k, v} <- map, into: %{} do
      {
        k,
        v
        |> U.debug(label: "before literal")
        |> :compiler.compile_literal()
        |> U.debug(label: "after literal")
        |> expand_main(k)
        |> :compiler.unroll()
        |> U.debug(label: "after unroll")
      }
    end
  end

  defp expand_main(v, :main) do
    v
    |> U.debug(label: "before expand")
    |> :expander.expand()
    |> U.debug(label: "after expand")
    |> :expander.step()
  end

  defp expand_main(v, _), do: v
end

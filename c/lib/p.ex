defmodule P do
  import NimbleParsec

  defp wrap_it(_rest, args, context, _line, _offset) do
    [k | v] = args |> Enum.drop(1) |> Enum.reverse() |> Enum.drop(1) |> Enum.map(&to_atom/1)
    {[{k, v}], context}
  end

  defp to_atom(str) when is_binary(str) do
    String.to_atom(str)
  end

  defp to_atom(x), do: x

  identifier = ascii_string([?a..?z, ?A..?Z, ?0..?9], min: 1)

  definition =
    string(":")
    |> ignore(string(" "))
    |> times(
      choice([integer(min: 1), identifier])
      |> times(ignore(choice([string("\n"), string(" ")])), min: 1),
      min: 1
    )
    |> string(";")
    |> repeat(ignore(choice([string("\n"), string(" ")])))
    |> lookahead(ignore(choice([eos(), string(":")])))
    |> post_traverse(:wrap_it)

  defparsec(:simple_forth, times(definition, min: 1))
end

defmodule P do
  import NimbleParsec

  defp wrap_it(_rest, args, context, _line, _offset) do
    [k | v] = args |> Enum.drop(1) |> Enum.reverse() |> Enum.drop(1) |> Enum.map(&to_type/1)
    {[{k, v |> List.flatten()}], context}
  end

  defp to_type({:binary, bin}), do: bin

  defp to_type(str) when is_binary(str) do
    String.to_atom(str)
  end

  defp to_type(x), do: x

  identifier = ascii_string([?a..?z, ?A..?Z, ?0..?9], min: 1)

  binary =
    ignore(string("\"")) |> utf8_string([not: 34], min: 1) |> ignore(string("\"")) |> tag(:binary)

  definition =
    string(":")
    |> ignore(string(" "))
    |> times(
      choice([integer(min: 1), identifier, binary])
      |> times(ignore(choice([string("\n"), string(" ")])), min: 1),
      min: 1
    )
    |> string(";")
    |> repeat(ignore(choice([string("\n"), string(" ")])))
    |> lookahead(ignore(choice([eos(), string(":")])))
    |> post_traverse(:wrap_it)

  defparsec(:simple_forth, times(definition, min: 1))
  defparsec(:binary, binary)
end

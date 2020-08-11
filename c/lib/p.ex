defmodule P do
  import NimbleParsec

  defp wrap_it(_rest, args, context, _line, _offset) do
    [k | v] = args |> Enum.drop(1) |> Enum.reverse() |> Enum.drop(1) |> Enum.map(&to_type/1)
    {[{k, v |> List.flatten() |> Enum.reject(&is_nil/1)}], context}
  end

  defp to_type({:binary, bin}), do: bin

  defp to_type({:hex_number, [hex]}) do
    {int, ""} = Integer.parse(hex, 16)
    int
  end

  defp to_type({:comment, x}), do: IO.inspect(x)

  defp to_type({:neg_integer, [int]}), do: -int

  defp to_type(str) when is_binary(str) do
    String.to_atom(str)
  end

  defp to_type(x), do: x

  identifier =
    ascii_string([?a..?z, ?A..?Z, ?0..?9, ?=, ?_, ?!, ?+, ?-, ?*, ?/, ?%, ?>, ?<, ?&, ?|, ?.],
      min: 1
    )

  neg_integer = ignore(string("-")) |> integer(min: 1) |> tag(:neg_integer)

  hex_number =
    ignore(string("0x"))
    |> ascii_string([?a..?f, ?A..?F, ?0..?9], min: 1)
    |> tag(:hex_number)

  comment =
    ignore(string("("))
    |> ascii_string([not: ?)], min: 0)
    |> ignore(string(")"))
    |> tag(:comment)

  binary =
    ignore(string("\"")) |> utf8_string([not: 34], min: 0) |> ignore(string("\"")) |> tag(:binary)

  definition =
    string(":")
    |> ignore(string(" "))
    |> times(
      choice([hex_number, integer(min: 1), neg_integer, identifier, binary])
      |> times(ignore(choice([string("\n"), string(" ")])), min: 1),
      min: 1
    )
    |> string(";")
    |> repeat(ignore(string(" ")))
    |> optional(ignore(comment))
    |> repeat(ignore(choice([string("\n"), string(" ")])))
    |> lookahead(ignore(choice([eos(), string(":")])))
    |> post_traverse(:wrap_it)

  defparsec(:simple_forth, times(definition, min: 1))
end

defmodule P do
  import NimbleParsec

  defp wrap_it(_rest, args, context, _line, _offset) do
    [k | v] =
      args
      # |> IO.inspect(label: "wrap")
      |> normalize_definition()

    {[{k, v |> List.flatten() |> Enum.reject(&is_nil/1)}], context}
  end

  defp normalize_definition(["inline" | d]) do
    [k | v] = normalize_definition(d)
    [k, :inline_start | v] ++ [:inline_end]
  end

  defp normalize_definition(d) do
    d |> Enum.drop(1) |> Enum.reverse() |> Enum.drop(1) |> Enum.map(&to_type/1)
  end

  defp to_type({:binary, bin}), do: bin

  defp to_type({:binary_list, list}), do: :erlang.list_to_binary(list)

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
    ascii_string(
      [?a..?z, ?A..?Z, ?0..?9, ?=, ?_, ?!, ?+, ?-, ?*, ?/, ?%, ?>, ?<, ?&, ?|, ?., ?[, ?]],
      min: 1
    )

  pos_integer = integer(min: 1) |> lookahead(string(" "))

  neg_integer =
    ignore(string("-")) |> integer(min: 1) |> lookahead(string(" ")) |> tag(:neg_integer)

  hex_number =
    ignore(string("0x"))
    |> ascii_string([?a..?f, ?A..?F, ?0..?9], min: 1)
    |> tag(:hex_number)

  comment =
    repeat(
      ignore(string("("))
      |> ascii_string([not: ?)], min: 0)
      |> ignore(string(")"))
      |> tag(:comment)
      |> repeat(ignore(choice([string("\n"), string(" ")])))
    )

  binary =
    ignore(string("\"")) |> utf8_string([not: 34], min: 0) |> ignore(string("\"")) |> tag(:binary)

  binary_list =
    ignore(string("<<"))
    |> repeat(
      integer(min: 1)
      |> optional(ignore(ascii_char([?,])))
      |> optional(ignore(string("\s")))
    )
    |> ignore(string(">>"))
    |> tag(:binary_list)

  definition =
    ignore(repeat(choice([string("\n"), string(" ")])))
    |> ignore(comment)
    |> string(":")
    |> ignore(string(" "))
    |> times(
      choice([hex_number, pos_integer, neg_integer, binary_list, identifier, binary])
      |> times(ignore(choice([string("\n"), string(" ")])), min: 1),
      min: 1
    )
    |> string(";")
    |> repeat(ignore(string(" ")))
    |> optional(string("inline"))
    |> optional(ignore(comment))
    |> repeat(ignore(choice([string("\n"), string(" ")])))
    |> optional(ignore(comment))
    |> lookahead(ignore(choice([eos(), string(":")])))
    |> post_traverse(:wrap_it)

  defparsec(:simple_forth, times(definition, min: 1) |> eos())
end

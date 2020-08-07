defmodule CTest do
  use ExUnit.Case
  doctest C

  test "greets the world" do
    assert C.hello() == :world
  end

  test "compile code" do
    code = """
    : left split drop ;

    : right split nip ;

    : sub
      rot rot right
      swap left ;

    """

    mid = %{
      left: [:split, :drop],
      right: [:split, :nip],
      sub: [:rot, :rot, :right, :swap, :left]
    }

    assert C.parse(code) == mid

    asm = %{
      left: [:split, :drop],
      right: [:split, :nip],
      sub: [:rot, :rot, :split, :nip, :swap, :split, :drop]
    }

    assert C.replace(mid) == asm
  end
end

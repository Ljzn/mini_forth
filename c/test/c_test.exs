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

    : substr
      rot rot right
      swap left ;

    : main "I am a fish" 2 2 substr ;

    """

    mid = %{
      left: [:split, :drop],
      right: [:split, :nip],
      substr: [:rot, :rot, :right, :swap, :left],
      main: ["I am a fish", 2, 2, :substr]
    }

    assert C.parse(code) == mid

    asm = %{
      left: [:split, :drop],
      right: [:split, :nip],
      substr: [:rot, :rot, :split, :nip, :swap, :split, :drop],
      main: ["I am a fish", 2, 2, :rot, :rot, :split, :nip, :swap, :split, :drop]
    }

    assert C.replace(mid) == asm

    assert {["am"], []} == :interpreter.eval(asm.main)
  end
end

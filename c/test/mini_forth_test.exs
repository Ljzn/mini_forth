defmodule MiniForthTest do
  use ExUnit.Case

  test "run code" do
    code = """
    : left split drop ;

    : right split nip ;

    : substr
      rot rot right
      swap left ;

    : main "I am a fish" 2 2 substr ;

    """

    assert {["am"], []} == MiniForth.run_code(code)
  end

  test "opcode tests" do
    File.read!("./test/opcode_test.fth")
    |> MiniForth.run_code()
  end
end

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

  test "macro tests: bi" do
    code = """
    : main 1 [ 1 + ] [ 2 + ] bi ;
    """

    assert "OP_1 OP_1 OP_ADD OP_1 OP_2 OP_ADD" == MiniForth.to_asm(code)
  end

  test "macro tests: compile time code" do
    code = """
    : simple_macro ( compile_time_arg runtime_arg -- a b )
    [ 3 + ]
    macro_start
        swap tas curry eval fas
    macro_end
    +
    ;

    : main 1 2 simple_macro ;
    """

    assert "04 OP_2 OP_ADD" == MiniForth.to_asm(code)
  end
end

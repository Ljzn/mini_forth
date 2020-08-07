defmodule CTest do
  use ExUnit.Case
  doctest C

  test "greets the world" do
    assert C.hello() == :world
  end
end

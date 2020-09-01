defmodule DTest do
  use ExUnit.Case
  doctest D

  test "greets the world" do
    assert D.hello() == :world
  end
end

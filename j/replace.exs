parser = File.read!("mini.js")
target = File.read!("source.html")
readme = File.read!("readme.fth")

result =
  target
  |> String.replace("XXXPARSERXXX", parser)
  |> String.replace("XXXREADMEXXX", readme)
  |> String.replace(
"""
module.exports = {
  SyntaxError: peg$SyntaxError,
  parse:       peg$parse
};
""", "")

File.write!("index.html", result)
IO.puts "done"

// Advent of Code Day 1
// Adam Gluck

@main
def dayOne(): Int = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input to solution
  content
    .split("\n") // Get lines in input files
    .map(_.filter(_.isDigit)) // Convert to numbers
    .filter(_.nonEmpty) // Don't count line if empty
    .map(x => s"${x.head}${x.last}") // Get first and last digits
    .map(_.toInt)
    .reduce((x, y) => x + y) // Sum all lines
}

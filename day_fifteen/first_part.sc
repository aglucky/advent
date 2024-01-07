// Advent of Code Day 15
// Adam Gluck

// Implement HASH algorithm on string
def hashString(str: String): Int = {
  var curVal = 0
  str.foldLeft(0)((acc: Int, x: Char) => ((acc + x.toInt)*17)%256)
}

@main
def dayFifteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  content.split(',').map(hashString).sum

}

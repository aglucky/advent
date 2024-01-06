// Advent of Code Day 13
// Adam Gluck

def isSymmetricAtIndex(str: String, index: Int): Boolean = {
  val (left, right) = str.splitAt(index)
  val maxLen = Math.min(left.length, right.length)
  left.reverse.take(maxLen) == right.take(maxLen)
}

def symmetryIndex(grid: List[String]): Int = {
  val columnIndex = (1 until grid.head.length)
    .find(i => grid.forall(isSymmetricAtIndex(_, i)))
    .getOrElse(0)

  val tranposedGrid = grid.transpose
  val rowIndex = (1 until tranposedGrid.head.length)
    .find(i => tranposedGrid.forall(row => isSymmetricAtIndex(row.mkString, i)))
    .getOrElse(0)

  columnIndex + 100 * rowIndex
}
@main
def dayThirteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  content
    .split("\n\n")
    .map(_.split('\n').toList)
    .map(symmetryIndex)
    .sum

}

// Advent of Code Day 13
// Adam Gluck

def hammingDistance(s1: String, s2: String): Int = {
  s1.zip(s2).count { case (c1, c2) => c1 != c2 }
}

def numDiffer(str: String, index: Int): Int = {
  val (left, right) = str.splitAt(index)
  val maxLen = Math.min(left.length, right.length)
  hammingDistance(left.reverse.take(maxLen), right.take(maxLen))
}

def symmetryIndex(grid: List[String]): Int = {
  val columnIndex = (1 until grid.head.length)
    .find(i =>
      grid.forall(numDiffer(_, i) < 2) &&
        (grid.map(numDiffer(_, i)).sum == 1)
    )
    .getOrElse(0)

  val tranposedGrid = grid.transpose
  val rowIndex = (1 until tranposedGrid.head.length)
    .find(i =>
      tranposedGrid.forall(row => numDiffer(row.mkString, i) < 2) &&
        (tranposedGrid.map(row => numDiffer(row.mkString, i)).sum == 1)
    )
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

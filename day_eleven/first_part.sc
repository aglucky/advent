// Advent of Code Day 11
// Adam Gluck

type Pos = (Int, Int)

@main
def dayEleven() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input to solution
  val rawGrid = content
    .split("\n")
    .map(_.toCharArray())

  // Expand Rows
  var expandRows: List[List[Char]] = List()
  for (i <- (0 until rawGrid.length))
    if (!rawGrid(i).contains('#')) {
      expandRows = expandRows.appended(rawGrid(i).toList)
    }
    expandRows = expandRows.appended(rawGrid(i).toList)

  // Expand Columns
  val transposedRows = expandRows.transpose
  var transposedCols: List[List[Char]] = List()
  for (i <- (0 until transposedRows.length))
    if (!transposedRows(i).contains('#'))
      transposedCols = transposedCols.appended(transposedRows(i).toList)
    transposedCols = transposedCols.appended(transposedRows(i).toList)

  // Convert back to original grid
  val grid = transposedCols.transpose

  // Get positions of each galaxy
  val galaxyCoords = for {
    (row, rowIndex) <- grid.zipWithIndex
    (char, colIndex) <- row.zipWithIndex if char == '#'
  } yield (rowIndex, colIndex)

  // Get every possible galaxy pair
  val crossProduct = for {
    (x1, y1) <- galaxyCoords
    (x2, y2) <- galaxyCoords
  } yield Set((x1, y1), (x2, y2))

  // Convert to sum of distances between distinct pairs
  crossProduct.distinct.filter(_.size > 1).foldLeft(0) {
    (acc: Int, pair: Set[Pos]) =>
      val List((x1, y1), (x2, y2)) = pair.toList
      acc + (x2 - x1).abs + (y2 - y1).abs
  }
}

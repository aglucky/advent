// Advent of Code Day 11
// Adam Gluck

type Pos = (Int, Int)

@main
def dayEleven() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input to solution
  val grid = content
    .split("\n")
    .map(_.toCharArray())

  // Find Empty Rows
  var emptyRows: List[Int] = List()
  for (i <- (0 until grid.length))
    if (!grid(i).contains('#')) {
      emptyRows = emptyRows.appended(i)
    }

  // Find Empty Cols
  var emptyColumns: List[Int] = List()
  val tranposedGrid = grid.transpose
  for (i <- (0 until tranposedGrid.length))
    if (!tranposedGrid(i).contains('#')) {
      emptyColumns = emptyColumns.appended(i)
    }

    
  // Helper function to increment an X or Y coordinate
  val increment: Int => List[Int] => Int = (index) => (checkList) => {
    val dilation = 1000000
    if (checkList.contains(index)) dilation
    else 1
  }

  // Calculate coordinates for galaxies with dilation
  var galaxyCoords: List[(Int, Int)] = List()
  var curX = 0
  for (x <- (0 until grid.length))
    var curY = 0
    for (y <- (0 until grid(x).length))
      if (grid(x)(y) == '#')
        galaxyCoords = galaxyCoords.appended((curX, curY))
      curY += increment(y)(emptyColumns)
    curX += increment(x)(emptyRows)


  // Get every possible galaxy pair
  val crossProduct = for {
    (x1, y1) <- galaxyCoords
    (x2, y2) <- galaxyCoords
  } yield Set((x1, y1), (x2, y2))

  // Convert to sum of distances between distinct pairs
  crossProduct.distinct.filter(_.size > 1).foldLeft(0L) {
    (acc: Long, pair: Set[Pos]) =>
      val List((x1, y1), (x2, y2)) = pair.toList
      acc + (x2 - x1).abs + (y2 - y1).abs
  }
}

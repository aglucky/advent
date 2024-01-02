// Advent of Code Day 3
// Adam Gluck

def retrieveFullNum(
    x: Int,
    y: Int,
    grid: Array[List[Char]],
    seen: Set[(Int, Int)]
): (Int, Set[(Int, Int)]) = {
  var updatedSeen = seen + ((x, y))
  val line = grid(x)
  var curNum: String = s"${line(y)}"

  // insert digits before input
  var i = y - 1
  while (i >= 0 && line(i).isDigit) {
    updatedSeen += ((x, i))
    curNum = line(i) + curNum
    i -= 1
  }

  // append digits before input
  i = y + 1
  while (i < line.length && line(i).isDigit) {
    updatedSeen += ((x, i))
    curNum += line(i)
    i += 1
  }

  (curNum.toInt, updatedSeen)
}

def getGearRatio(
    x: Int,
    y: Int,
    grid: Array[List[Char]],
    seen: Set[(Int, Int)]
): (Int, Set[(Int, Int)]) = {
  var list = List[Int]()
  var updatedSeen = seen

  // Add full digit if grid(x)(y) is digit, is in range, and not seen before
  def addIfValid(x: Int, y: Int) = {
    if (
      x >= 0 && x < grid.length &&
      y >= 0 && y < grid(x).length &&
      grid(x)(y).isDigit &&
      !(updatedSeen.contains(x, y))
    ) {
      val (num, newSeen) = retrieveFullNum(x, y, grid, updatedSeen)
      list = num :: list
      updatedSeen = newSeen
    }
  }

  // Check every grid pos in surrounding 3 by 3 square
  for (dx <- -1 to 1; dy <- -1 to 1) {
    addIfValid(x + dx, y + dy)
  }

  // Set gearRatio to product of surrounding nums if exactly 2
  var gearRatio = 0
  if (list.length == 2) {
    gearRatio = list.product
  }

  (gearRatio, updatedSeen)
}

@main
def dayThree(): Int = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input to 2d grid
  val grid = content
    .split("\n")
    .map(_.toCharArray.toList)

  var sum = 0
  var seen = Set[(Int, Int)]()

  for (x <- 0 until grid.length) do
    for (y <- 0 until grid(x).length) do
      if (grid(x)(y) == '*') {
        val (gearRatio, updatedSeen) = getGearRatio(x, y, grid, seen)
        sum += gearRatio
        seen = updatedSeen
      }

  return sum
}

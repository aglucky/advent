// Advent of Code Day 10
// Adam Gluck

type Grid = List[List[Char]]
type Pos = (Int, Int)

def isInBounds(pos: Pos, grid: Grid): Boolean = {
  val (x, y) = pos
  x >= 0 && y >= 0 && x < grid.length && y < grid(x).length
}

def getFirstPipe(start: Pos, grid: Grid): (Pos, Pos) = {
  val (x, y) = start
  val validUp = Set('F', '7', '|')
  val validDown = Set('J', 'L', '|')
  val validLeft = Set('L', 'F', '-')
  val validRight = Set('7', 'J', '-')

  val (dx, dy, posSet) = List(
    (1, 0, validUp),
    (-1, 0, validDown),
    (0, 1, validRight),
    (0, -1, validLeft)
  ).find((triple) => {
    val (dx, dy, checkSet) = triple
    val (newX, newY) = (x + dx, y + dy)

    isInBounds((newX, newY), grid) && checkSet.contains(grid(newX)(newY))
  }).getOrElse((-1, -1, Set()))

  ((x + dx, y + dy), (dx, dy))
}

def getLoopLen(start: Pos, dPos: Pos, grid: Grid): Int = {
  var curPos = start
  var curDir = dPos
  var length = 1

  while ((grid(curPos._1)(curPos._2) != 'S'))
    val (x, y) = curPos
    length += 1

    grid(x)(y) match
      case 'S' => length
      case '7' => {
        curDir match
          case (0, 1) => {
            curPos = (x + 1, y)
            curDir = (1, 0)
          }
          case (-1, 0) => {
            curPos = (x, y - 1)
            curDir = (0, -1)
          }
      }
      case 'J' => {
        curDir match
          case (0, 1) => {
            curPos = (x - 1, y)
            curDir = (-1, 0)
          }
          case (1, 0) => {
            curPos = (x, y - 1)
            curDir = (0, -1)
          }
      }
      case 'F' => {
        curDir match
          case (0, -1) => {
            curPos = (x + 1, y)
            curDir = (1, 0)
          }
          case (-1, 0) => {
            curPos = (x, y + 1)
            curDir = (0, 1)
          }
      }
      case 'L' => {
        curDir match
          case (0, -1) => {
            curPos = (x - 1, y)
            curDir = (-1, 0)
          }
          case (1, 0) => {
            curPos = (x, y + 1)
            curDir = (0, 1)
          }
      }
      case '-' => {
        curDir match
          case (0, -1) => {
            curPos = (x, y - 1)
            curDir = (0, -1)
          }
          case (0, 1) => {
            curPos = (x, y + 1)
            curDir = (0, 1)
          }
      }
      case '|' => {
        curDir match
          case (-1, 0) => {
            curPos = (x - 1, y)
            curDir = (-1, 0)
          }
          case (1, 0) => {
            curPos = (x + 1, y)
            curDir = (1, 0)
          }
      }
  length

}

@main
def dayTen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  val grid: Grid =
    content.split("\n").map(_.toCharArray().toList).toList

  var start = (-1, -1)
  for (x <- (0 until grid.length))
    for (y <- (0 until grid(x).length))
      if (grid(x)(y) == 'S')
        start = (x, y)

  val (firstPipe, dPos) = getFirstPipe(start, grid)
  getLoopLen(firstPipe, dPos, grid) / 2
}

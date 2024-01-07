// Advent of Code Day 16
// Adam Gluck

val energized = Set('#', '!', '~', 'v', '^')

def updateGrid(
    grid: Array[Array[Char]],
    pos: (Int, Int),
    char: Char = '#'
): Unit = {
  val (x, y) = pos
  grid(x)(y) = char
}


def shineLight(
    grid: Array[Array[Char]],
    startPos: (Int, Int),
    startDir: (Int, Int)
): Unit = {
  val stack = scala.collection.mutable.Stack[((Int, Int), (Int, Int))]()
  val visited = scala.collection.mutable.Set[((Int, Int), (Int, Int))]()
  
  stack.push((startPos, startDir))

  while (stack.nonEmpty) {
    val ((x, y), (dx, dy)) = stack.pop()
    val state = ((x, y), (dx, dy))

    if (
      !((x < 0) || (x >= grid.length) || (y < 0) || (y >= grid(x).length)) && 
      !visited.contains(state)
    ) {
      visited.add(state)
      grid(x)(y) match {
        case '.' | '#' =>
          updateGrid(grid, (x, y))
          stack.push(((x + dx, y + dy), (dx, dy)))
        case '|' | '!' =>
          val updateChar = '!'
          updateGrid(grid, (x, y), updateChar)
          if (dy == 0) {
            stack.push(((x + dx, y + dy), (dx, dy)))
          } else {
            stack.push(((x - 1, y), (-1, 0)))
            stack.push(((x + 1, y), (1, 0)))
          }
        case '-' | '~' =>
          val updateChar = '~'
          updateGrid(grid, (x, y), updateChar)
          if (dx == 0) {
            stack.push(((x + dx, y + dy), (dx, dy)))
          } else {
            stack.push(((x, y - 1), (0, -1)))
            stack.push(((x, y + 1), (0, 1)))
          }
        case '/' | '^' =>
          val updateChar = '^'
          updateGrid(grid, (x, y), updateChar)
          stack.push(((x - dy, y - dx), (-dy, -dx)))
        case '\\' | 'v' =>
          val updateChar = 'v'
          updateGrid(grid, (x, y), updateChar)
          stack.push(((x + dy, y + dx), (dy, dx)))
        case _ =>
      }
    }
  }
}

@main
def daySixteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  val grid = content.split('\n').map(_.toCharArray())

  shineLight(grid, (0, 0), (0, 1))

  grid.flatMap(str =>
    str.map(char => {
        char match
          case c if energized.contains(c) => 1
          case _                          => 0
      })).sum
}

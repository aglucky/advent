// Advent of Code Day 16
// Adam Gluck

val doubleEnergy = Set('2', '1', '+', 'N', 'Y')
val singleEnergy = Set('#', 'X', '!', '?', '~', '=', 'v', 'V', '^', '%')

def updateGrid(
    grid: List[String],
    pos: (Int, Int),
    char: Char = '#'
): List[String] = {
  val (x, y) = pos
  grid.updated(x, grid(x).updated(y, char))
}

def shineLight(
    grid: List[String],
    pos: (Int, Int),
    dir: (Int, Int),
    visited: Set[((Int, Int), (Int, Int))] = Set.empty
): List[String] = {
  val (x, y) = pos
  val (dx, dy) = dir
  val state = (pos, dir)
  if (
    ((x < 0) || (x >= grid.length) || (y < 0) || (y >= grid(x).length)) ||
    (visited.contains(state))
  )
    grid
  else {
    val newVisited = visited + state
    grid(x)(y) match
      case '.' | '#' | 'X' | '2' => {
        val updateChar = (grid(x)(y), dx, dy) match {
          case ('2', _, _) | ('#', 0, _) | ('X', _, 0) => '2'
          case (_, _, 0)                               => '#'
          case (_, 0, _)                               => 'X'
        }
        shineLight(
          updateGrid(grid, pos, updateChar),
          (x + dx, y + dy),
          dir,
          newVisited
        )
      }
      case '|' | '!' | '?' | '1' => {
        val updateChar = (grid(x)(y), dx, dy) match {
          case ('1', _, _) | ('!', 0, _) | ('?', _, 0) => '1'
          case (_, _, 0)                               => '!'
          case (_, 0, _)                               => '?'
        }
        dy match
          case 0 =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x + dx, y + dy),
              dir,
              newVisited
            )
          case _ => {
            val topGrid =
              shineLight(
                updateGrid(grid, pos, updateChar),
                (x - 1, y),
                (-1, 0),
                newVisited
              )
            shineLight(
              updateGrid(topGrid, pos, updateChar),
              (x + 1, y),
              (1, 0),
              newVisited
            )
          }
      }
      case '-' | '~' | '=' | '+' => {
        val updateChar = (grid(x)(y), dx, dy) match {
          case ('+', _, _) | ('~', 0, _) | ('=', _, 0) => '+'
          case (_, _, 0)                               => '~'
          case (_, 0, _)                               => '='
        }
        dx match
          case 0 =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x + dx, y + dy),
              dir,
              newVisited
            )
          case _ => {
            val leftGrid =
              shineLight(
                updateGrid(grid, pos, updateChar),
                (x, y - 1),
                (0, -1),
                newVisited
              )
            shineLight(
              updateGrid(leftGrid, pos, updateChar),
              (x, y + 1),
              (0, 1),
              newVisited
            )
          }
      }
      case '/' | '^' | '%' | 'N' => {
        val updateChar = (grid(x)(y), dx, dy) match {
          case ('N', _, _) | ('^', 0, _) | ('%', _, 0) => 'N'
          case (_, _, 0)                               => '^'
          case (_, 0, _)                               => '%'
        }
        dir match
          case (0, 1) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x - 1, y),
              (-1, 0),
              newVisited
            )
          case (0, -1) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x + 1, y),
              (1, 0),
              newVisited
            )
          case (1, 0) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x, y - 1),
              (0, -1),
              newVisited
            )
          case (-1, 0) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x, y + 1),
              (0, 1),
              newVisited
            )
      }
      case '\\' | 'v' | 'V' | 'Y' => {
        val updateChar = (grid(x)(y), dx, dy) match {
          case ('Y', _, _) | ('v', 0, _) | ('V', _, 0) => 'Y'
          case (_, _, 0)                               => 'v'
          case (_, 0, _)                               => 'V'
        }
        dir match
          case (0, 1) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x + 1, y),
              (1, 0),
              newVisited
            )
          case (0, -1) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x - 1, y),
              (-1, 0),
              newVisited
            )
          case (1, 0) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x, y + 1),
              (0, 1),
              newVisited
            )
          case (-1, 0) =>
            shineLight(
              updateGrid(grid, pos, updateChar),
              (x, y - 1),
              (0, -1),
              newVisited
            )
      }
      case _ => grid
  }
}

@main
def daySixteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "test.txt"
  val content: String = os.read(input_path)

  val grid = content.split('\n').toList
  shineLight(grid, (0, 0), (0, 1))
  .map(str =>
    str.map(char => {
      char match
        case c if doubleEnergy.contains(c) => 1
        case c if singleEnergy.contains(c) => 1
        case _ => 0
    }).map(_ match {
      case 2 => '2'
      case 1 => '#'
      case _ => '.'
    }).mkString
  )
}

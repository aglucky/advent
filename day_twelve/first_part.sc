// Advent of Code Day 12
// Adam Gluck

def isValid(gears: List[Char], conditions: List[Int]): Boolean = {
  val rep = gears.mkString
    .split('.')
    .filter(_.length() > 0)
    .map(_.length())
    .toList

  rep == conditions
}

def isPossible(gears: List[Char], conditions: List[Int], index: Int): Boolean = {
  val rep = gears.slice(0, index+1).mkString
    .split('.')
    .filter(_.length() > 0)
    .map(_.length())
    .toList

  conditions.startsWith(rep.take(rep.length-1))
}

def findWays(gears: List[Char], conditions: List[Int]): Int = {

  val start = gears.indexOf('?')
  start match
    case -1 if isValid(gears, conditions) => 1
    case -1 => 0
    case index  if isPossible(gears, conditions, index) => findWays(gears.updated(start, '.'), conditions) + findWays(gears.updated(start, '#'), conditions)
    case _ => 0
}

@main
def dayTwelve() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input to solution
  val records: List[(List[Char], List[Int])] = content
    .split("\n")
    .map((line) => {
      line.split(" ") match
        case Array(springs, conditions) => {
          val springList = springs.toCharArray.toList
          val conditionList = conditions
            .split(",")
            .map(_.filter(_.isDigit))
            .filter(_.nonEmpty)
            .map(_.toInt)
            .toList

          (springList, conditionList)
        }
        case _ => (List[Char](), List[Int]())
    })
    .toList

  records.map((pair) => {
    val (gears, conditions) = pair
    findWays(gears, conditions)
  }).sum
}

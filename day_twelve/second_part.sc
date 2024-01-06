// Advent of Code Day 12
// Adam Gluck

type Input = (String, List[Int])

def findWaysMemo(gears: String, conditions: List[Int]): Long = {
  // Set up memo empty for each call
  val memo = scala.collection.mutable.Map[Input, Long]()

  def findWays(gears: String, conditions: List[Int]): Long = {
    memo.getOrElseUpdate(
      (gears, conditions), {
        conditions match {
          case condition :: tailConditions if (gears.length >= condition) => {
            gears.head match {
              // Ignore and check rest
              case '.' => findWays(gears.tail, conditions)
              // Check if first group is valid and check rest
              case '#' =>
                val (group, rest) = gears.splitAt(condition)
                if (group.contains('.') || rest.headOption.contains('#'))
                  0L // Condition is not met
                else
                  findWays( // Check remaining conditions
                    "." + rest.tail,
                    tailConditions
                  )
                // Branch out on both possibilities
              case '?' =>
                findWays(s".${gears.tail}", conditions) +
                  findWays(s"#${gears.tail}", conditions)

            }
          }
          // Determine if string is valid after all conditions met
          case Nil => if (gears.contains('#')) 0L else 1L
          // Invalid string if shorter than group
          case _ => 0L
        }
      }
    )
  }

  findWays(gears, conditions)
}

def unfoldPair(pair: Input): Input = {
  val (gears, conditions) = pair
  val unfoldedGears = (gears + '?') * 4 + gears
  val unfoldedConditions = List.fill(5)(conditions).flatten
  (unfoldedGears, unfoldedConditions)
}

@main
def dayTwelve() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input to data structure
  val records: List[Input] = content
    .split("\n")
    .map((line) => {
      line.split(" ") match
        case Array(springs, conditions) => {
          val conditionList = conditions
            .split(",")
            .map(_.filter(_.isDigit))
            .filter(_.nonEmpty)
            .map(_.toInt)
            .toList

          (springs, conditionList)
        }
        case _ => (String(), List[Int]())
    })
    .toList

  records
    .map(unfoldPair(_))
    .map((pair) => {
      val (gears, conditions) = pair
      findWaysMemo(gears, conditions)
    })
    .sum
}

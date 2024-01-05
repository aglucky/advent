// Advent of Code Day 12
// Adam Gluck

def findNextIndex(gears: List[Char], numBroken: Int) = {
  gears.indexOf('#') match
    case -1    => gears.length - 1
    case index => index + numBroken + 1
}

def conditionHelper(
    gears: List[Char],
    numBroken: Int,
    charIndexMap: ((Char, Int)) => Int
): Boolean = {
  val brokenIndexes = gears.zipWithIndex
    .map(charIndexMap)
    .filter(_ >= 0)

  brokenIndexes.length >= numBroken &&
  brokenIndexes
    .slice(0, numBroken)
    .sliding(2)
    .forall(pair => pair.length < 2 || (pair(1) - pair(0)) == 1) &&
  (brokenIndexes.length <= numBroken ||
  (gears(brokenIndexes(numBroken)) != '#')
  )
}

def isPossible(gears: List[Char], numBroken: Int): Boolean = {
  conditionHelper(
    gears,
    numBroken,
    {
      case ('?', index) => index
      case ('#', index) => index
      case _            => -1
    }
  )
}

def isMet(gears: List[Char], numBroken: Int): Boolean = {
  conditionHelper(
    gears,
    numBroken,
    {
      case ('#', index) => index
      case _            => -1
    }
  )
}

def findArrangements(gears: List[Char], conditions: List[Int]): Int =
  conditions match {
    case constraint :: next => {
      println(conditions)
      var numWays = 0
      val index = gears.indexOf('?')
      if (index >= 0)
        val possibilities =
          List(gears.updated(index, '#'), gears.updated(index, '.'))
        numWays += possibilities
          .map((newGears) => {
            isPossible(newGears, constraint) match
              case true => {
                println(newGears)
                isMet(newGears, constraint) match
                  case true => {
                    println("Is met")
                    val numPos = findArrangements(
                      newGears.slice(
                        findNextIndex(newGears, constraint),
                        newGears.length
                      ),
                      next
                    )
                    numPos
                  }
                  case false => {
                    val numPos = findArrangements(newGears, conditions)
                    numPos
                  }
              }
              case false => 0
          })
          .sum
      numWays
    }
    case Nil => {
      println(1)
      gears.count(_ == '?') + 1
    }
  }

@main
def dayTwelve() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "test.txt"
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

  val (gears, conditions) = records(0)
  findArrangements(gears, conditions)
}

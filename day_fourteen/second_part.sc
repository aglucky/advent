// Advent of Code Day 14
// Adam Gluck

// Tilt a string left
def tiltLeft(str: String): String = {
  var numSpaces = 0
  var tilted = List[Char]()
  str.foreach((char) => {
    char match
      case '#' => {
        tilted = tilted.appendedAll("." * numSpaces)
        tilted = tilted.appended('#')
        numSpaces = 0
      }
      case 'O' => {
        tilted = tilted.appended('O')
      }
      case _ => {
        numSpaces += 1
      }

  })

  tilted.appendedAll("." * numSpaces).mkString
}

// Perform a tilt cycle on grid
def cycle(grid: List[String]): List[String] = {
  val northTilt =
    grid.transpose
      .map(row => {
        tiltLeft(row.mkString)
      })
      .transpose

  val westTilt =
    northTilt.map(row => {
      tiltLeft(row.mkString)
    })

  val southTilt =
    westTilt.transpose
      .map(row => {
        tiltLeft(row.mkString.reverse).reverse
      })
      .transpose

  val eastTilt =
    southTilt.map(row => {
      tiltLeft(row.mkString.reverse).reverse
    })

  eastTilt
}

// Repeat tilt cycle n times
def repeatCycle(grid: List[String], n: Int): List[String] = {
  if (n <= 0) grid
  else repeatCycle(cycle(grid), n - 1)
}

// Calculate load from grid
def getLoad(grid: List[String]): Int = {
  grid.transpose
    .map(row => {
      row.zipWithIndex
        .filter(_._1 == 'O')
        .map(row.length - _._2)
        .sum
    })
    .sum
}

// Get first index of cycle
def findFirstCycle(grid: List[String], range:(Int, Int) = (150, 200)): (List[Int], Int) = {
  // Get sums between range
  val (start, endInclusive) = range
  val sumList = (start to endInclusive).map(repeatCycle(grid, _)).map(getLoad)

  // Find first cycle and start index
  val seen = scala.collection.mutable.Map[Int, Int]()
  var cycleStart = -1
  var cycle = List[Int]()
  sumList.zipWithIndex.find { case (sum, index) =>
    seen.get(sum) match
      case Some(startIndex) =>
        val sliceLength = index - startIndex
        if sumList.slice(startIndex, index) == sumList.slice(
            index,
            index + index - startIndex
          )
        then {
          cycleStart = startIndex
          cycle = sumList.slice(cycleStart, index).toList
          true
        } else {
          false
        }
      case _ => {
        seen(sum) = index
        false
      }
  }
  (cycle, cycleStart + start)
}

// Get nth sum given a pattern
def findSumWithPatten(index: Long, pattern: (List[Int], Int)): Int = {
  val (cycle, start) = pattern
  return cycle(((index - start) % cycle.length).toInt)
}

@main
def dayFourteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  val grid = content.split('\n').toList
  val pattern = findFirstCycle(grid)
  findSumWithPatten(1000000000L, pattern)
}

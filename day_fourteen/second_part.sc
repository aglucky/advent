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

// Take a given pattern to find nth element in O(1)
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

  // Cycle found in input data
  val inputPattern = (List(
    106390, 106374, 106350, 106332, 106337, 106336, 106351, 106369, 106390,
    106404, 106398, 106389, 106375, 106349, 106333, 106336, 106337, 106350,
    106370, 106389, 106405, 106397
  ), 120)

  // Cycle found in test data
  val testPattern = (List(
    63, 68, 69, 69, 65, 64, 65
  ), 120)

  // Find cycle by hand and use pattern to find desired index
  findSumWithPatten(1000000000L, inputPattern)

}

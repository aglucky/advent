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

@main
def dayFourteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  val grid = content.split('\n').toList

  // Tilt grid north and calculate load
  grid.transpose
    .map(row => {
      tiltLeft(row.mkString)
      .zipWithIndex
      .filter(_._1 == 'O')
      .map(row.length - _._2)
      .sum
    }).sum

}

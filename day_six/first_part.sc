// Advent of Code Day 6
// Adam Gluck

def findWays(time: Int, dist: Int): Int = {
  val accel = 1
  def condition(t: Int): Boolean = (time - t) * t * accel >= dist

  (0 to time).filter(condition(_)).length
}

@main
def daySix() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Process input into lists
  var timeList: List[Int] = List()
  var distList: List[Int] = List()
  content.split("\n") match
    case Array(timeStr, distStr, _*) => {
      timeList = timeStr
        .split(" ")
        .map(_.filter(_.isDigit))
        .filter(_.length > 0)
        .map(_.toInt)
        .toList

      distList = distStr
        .split(" ")
        .map(_.filter(_.isDigit))
        .filter(_.length > 0)
        .map(_.toInt)
        .toList
    }

    case _ =>
      throw new IllegalArgumentException("Invalid numbers formatting")

  timeList.zip(distList).map((x) => findWays(x._1, x._2)).product
}

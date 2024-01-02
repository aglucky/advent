// Advent of Code Day 6
// Adam Gluck

def findWays(time: Long, dist: Long): (Long, Long) = {
  val accel = 1L
  def condition(t: Long): Boolean = (time - t) * t * accel >= dist

  def binarySearch(min: Long, max: Long, searchForMax: Boolean): Long = {
    var start = min
    var end = max
    var answer = if (searchForMax) min else max

    while (start <= end) {
      val mid = start + (end - start) / 2
      if (condition(mid)) {
        answer = mid
        if (searchForMax) start = mid + 1 else end = mid - 1
      } else {
        if (searchForMax) end = mid - 1 else start = mid + 1
      }
    }
    answer
  }

  // Binary search for bounds
  val minTime = binarySearch(0, time, searchForMax = false)
  val maxTime = binarySearch(0, time, searchForMax = true)

  (minTime, maxTime)
}

@main
def daySix() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Process input into lists
  var timeList: List[Long] = List()
  var distList: List[Long] = List()
  content.split("\n") match
    case Array(timeStr, distStr, _*) => {
      timeList = timeStr
        .split(" ")
        .map(_.filter(_.isDigit))
        .filter(_.length > 0)
        .map(_.toLong)
        .toList

      distList = distStr
        .split(" ")
        .map(_.filter(_.isDigit))
        .filter(_.length > 0)
        .map(_.toLong)
        .toList
    }

    case _ =>
      throw new IllegalArgumentException("Invalid numbers formatting")

  val (min, max) = findWays(timeList.mkString.toLong, distList.mkString.toLong)
  max - min + 1 // Get elements in range
}

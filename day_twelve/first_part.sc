// Advent of Code Day 12
// Adam Gluck

def findArrangements(gears: List[Char], conditions: List[Int]): Int = {

  conditions match
    case head :: next => {
      var sum = 0
      for ((char, index) <- gears.zipWithIndex if char == '?')
        val damaged = gears.updated(index, '#')
        val fixed = gears.updated(index, '.')
        sum+=1
      sum
    }
    case Nil => gears.count(_ == '?') + 1
  
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
  
    val (gears, conditions) =records(0)
    println(gears)
    println(conditions)

}

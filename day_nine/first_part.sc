// Advent of Code Day 9
// Adam Gluck

def getNext(hist: List[Int]): Int = {
  hist match 
    case List(0) => 0
    case List(el) => el
    case _ => {
      val diffs = hist.sliding(2).map(x => x(1) - x(0)).toList
      hist.last + getNext(diffs)
    }
}

@main
def dayNine() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  val histories: List[List[Int]] =
    content.split("\n").map(_.split(" ").map(_.toInt).toList).toList
  
  histories.map(getNext).sum
}

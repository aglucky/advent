// Advent of Code Day 2
// Adam Gluck

// Class to represent a hand in a game
class Hand(var blue: Int, var green: Int, var red: Int):
  // Alternative constructor to create hand from string
  def this(line: String) = {
    this(0, 0, 0)
    // Function to find val for a given color
    def processColor(
        colors: Array[String],
        colorToFind: String,
        setColorVal: Int => Unit
    ): Unit = {
      val num = colors
        .collectFirst {
          case colorStr if colorStr.contains(colorToFind) =>
            colorStr.filter(_.isDigit).toInt
        }
        .getOrElse(0)

      setColorVal(num)
    }

    // Process each color if they are of type string
    line.split(",") match
      case list: Array[String] => {
        processColor(list, "blue", (x) => (this.blue = x))
        processColor(list, "red", (x) => (this.red = x))
        processColor(list, "green", (x) => (this.green = x))
      }
      case null => throw new IllegalArgumentException("Invalid hand format")

  }

  // Check if hand is possible given a sample hand
  def isSupersetOf(hand2: Hand): Boolean = {
    this.blue >= hand2.blue && this.green >= hand2.green && this.red >= hand2.red
  }

  override def toString(): String = {
    return s"${this.blue} blue, ${this.green} green, ${this.red} red"
  }

// Class to represent a game
class Game(var id: Int, var hands: Array[Hand]):

  // Alternate constructor to create game from line of input
  def this(line: String) = {
    this(0, Array())

    line.split(":") match
      case Array(idStr: String, handStr: String) => {
        this.id = idStr.filter(_.isDigit).toInt
        this.hands = handStr.split(";").map(Hand(_))
      }
      case _ => throw new IllegalArgumentException("Invalid game format")
  }

  // Return id if every hand is possible given an input hand
  def getValidId(maxHand: Hand): Int = {
    if (this.hands.forall(maxHand.isSupersetOf(_))) this.id
    else 0
  }

  override def toString(): String = {
    return s"Game ${this.id}: ${this.hands.mkString(";")}"
  }

@main
def dayTwo(): Int = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  val validHand = Hand(14,13,12)

  // Convert input to solution
  content
    .split("\n")
    .map(Game(_))
    .map(_.getValidId(validHand))
    .reduce((acc, x) => (acc + x))
}

// Advent of Code Day 7
// Adam Gluck

// Global for card values
val cardOrder = Map('A' -> 13, 'K' -> 12, 'Q' -> 11, 'J' -> 10, 'T' -> 9, '9' -> 8, '8' -> 7, '7' -> 6, '6' -> 5, '5' -> 4, '4' -> 3, '3' -> 2, '2' -> 1)

// Classify hand in an ordered enum
object HandType extends Enumeration {
  val HighCard, OnePair, TwoPair, ThreeOfAKind, FullHouse, FourOfAKind, FiveOfAKind = Value
}

import HandType._ // Allow us to use "FiveOfAKind" instead of "HandType.FiveOfAKind"

// Classify hand into a HandType
def getType(hand: String): HandType.Value = {
  val grouped = hand.groupBy(identity).mapValues(_.length).toList.sortBy(-_._2)
  grouped match {
    case (_, 5) :: Nil => FiveOfAKind
    case (_, 4) :: _ => FourOfAKind
    case (_, 3) :: (_, 2) :: Nil => FullHouse
    case (_, 3) :: _ => ThreeOfAKind
    case (_, 2) :: (_, 2) :: _ => TwoPair
    case (_, 2) :: _ => OnePair
    case _ => HighCard
  }
}

// Sort hands by type, then tyebreaker
val sortHands: Ordering[(String, Int)] = Ordering.fromLessThan { (a, b) =>
  val (handA, _) = a
  val (handB, _) = b
  val typeA = getType(handA)
  val typeB = getType(handB)
  if (typeA != typeB) {
    typeA > typeB
  } else {
    // Return card based on first highest
    val rankA = handA.map(cardOrder).toList
    val rankB = handB.map(cardOrder).toList
    rankA.zip(rankB).dropWhile { case (x, y) => x == y } match {
      case (x, y) :: _ => x > y
      case Nil => false
    }
  }
}

@main
def daySeven() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  var hands = content.split("\n").map((line) => line.split(" ")).collect {
    case Array(hand, bid) => (hand, bid.toInt)
  }

  hands
    .sorted(sortHands).reverse
    .zipWithIndex
    .map((pair, index) => ((index + 1) * pair._2))
    .sum
}

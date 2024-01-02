// Advent of Code Day 5
// Adam Gluck

// Get possible output ranges for seed range
def rangeConversion(
    seedRange: (Long, Long),
    conversionList: List[(Long, Long, Long)]
): List[(Long, Long)] = {
  val (start, end) = seedRange
  var before = (start, end)
  var after = (start, end)
  var possibleRanges = conversionList
    .collect {
      case (dest, source, range)
          if (
            start <= (source + range) &&
              end >= source
          ) => {
        val change = dest - source
        val convertedStart = Math.max(start, source)
        val convertedEnd = Math.min(end, (source + range - 1))
        before = (before._1, Math.min(convertedStart - 1, before._2))
        after = (Math.max(convertedEnd + 1, after._1), after._2)
        (convertedStart + change, convertedEnd + change)
      }
    }


  (possibleRanges, before, after) match {
    case (Nil, _, _) => List(seedRange) //Return input if no possible conversions
    case (_, b, a) =>
      //Add ranges not converted
      (if (b._2 >= b._1) List(b) else Nil) ++
      (if (a._2 >= a._1) List(a) else Nil) ++
      possibleRanges
  }
}

// Apply conversions to all seeds
def chainRangeConversions(
    seedRanges: List[(Long, Long)],
    conversions: List[List[(Long, Long, Long)]]
): List[(Long, Long)] = {
  seedRanges.flatMap { seedRange =>
    conversions.foldLeft(List(seedRange)) { (currentRanges, conversion) =>
      currentRanges.flatMap(rangeConversion(_, conversion))
    }
  }
}

@main
def dayFive() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input into data structures
  var seeds: List[(Long, Long)] = List()
  var conversions: List[List[(Long, Long, Long)]] = List()
  content.split("\n\n").toList match
    case (head: String) :: (rest: List[String]) => {
      seeds = head
        .split(" ")
        .map(_.filter(_.isDigit))
        .filter(_.length > 1)
        .map(_.toLong)
        .grouped(2)
        .map { case Array(start, range) =>
          (start, start + range - 1)
        }
        .toList

      conversions = rest.map((group) =>
        group
          .split("\n")
          .map((line) => {
            line.split(" ") match
              case Array(dest, source, range) =>
                Some((dest.toLong, source.toLong, range.toLong))
              case _ => None
          })
          .collect { case Some(value) => value }
          .toList
      )
    }
    case _ =>
      throw new IllegalArgumentException("Invalid input format")

  // Calculate solution
  chainRangeConversions(seeds, conversions).map((range) => range._1).min
}

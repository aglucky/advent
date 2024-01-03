// Advent of Code Day 5
// Adam Gluck

type Range = (Long, Long) // start and end are both inclusive
type SeedInput =
  (
      Long,
      Long,
      Long
  ) // in form (dest, start, range) where (start, start+range-1) -> (dest, end+range-1)

// Convert individual seeds for part 1
def chainSeedConversions(
    seed: Long,
    conversions: List[List[SeedInput]]
): Long = {
  conversions.foldLeft(seed)((curVal, conversion) => {
    conversion
      .collectFirst {
        case (dest, source, range)
            if ((curVal >= source) && (curVal < source + range)) =>
          curVal + (dest - source)
      }
      .getOrElse(curVal)
  })
}

@main
def dayFive() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Convert input into data structures
  var seeds: List[Long] = List()
  var conversions: List[List[SeedInput]] = List()
  content.split("\n\n").toList match
    case (head: String) :: (rest: List[String]) => {
      seeds = head
        .split(" ")
        .map(_.filter(_.isDigit))
        .filter(_.length > 1)
        .map(_.toLong)
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
  val convert = chainSeedConversions(_, conversions)
  seeds.map(convert).min
}

// Advent of Code Day 15
// Adam Gluck

type Lens = (String, Int)

// Implement HASH algorithm on string
def hashString(str: String): Int = {
  var curVal = 0
  str.foldLeft(0)((acc: Int, x: Char) => ((acc + x.toInt) * 17) % 256)
}

def addOrUpdate(buckets: List[List[Lens]], pair: Lens): List[List[Lens]] = {
  val (label, value) = pair
  val hashKey = hashString(label)

  // Determine if label is within bucket
  val replaceIndex =
    buckets(hashKey).zipWithIndex
      .find(_._1._1 == label)
      .map(_._2)
      .getOrElse((-1))

  // Add or modify label for bucket
  replaceIndex match
    case -1 => buckets.updated(hashKey, buckets(hashKey).appended(pair))
    case _ =>
      buckets.updated(hashKey, buckets(hashKey).updated(replaceIndex, pair))
}

def removeLabel(buckets: List[List[Lens]], label: String): List[List[Lens]] = {
  val hashKey = hashString(label)

  // Filter out desired label
  buckets.updated(hashKey, buckets(hashKey).filterNot(_._1 == label))

}

@main
def dayFifteen() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Set up buckets
  val numBuckets = 256
  var buckets = List.fill(numBuckets)(List[Lens]())

  // Manipulate buckets based on input
  content
    .split(',')
    .foreach((str) => {
      str.split("[-=]") match
        case Array(label, num) => {
          val pair = (label, num.toInt)
          buckets = addOrUpdate(buckets, pair)
        }
        case Array(label) => {
          buckets = removeLabel(buckets, label)
        }
    })

  // Get values from buckets
  buckets.zipWithIndex
    .filter(_._1.length > 0)
    .map((bucketWithIndex) => {
      val (bucket, bIndex) = bucketWithIndex
      bucket.zipWithIndex
        .map((lensWithIndex) => {
          val (lens, lIndex) = lensWithIndex
          (bIndex + 1) * (lIndex + 1) * (lens._2)
        })
        .sum
    })
    .sum

}

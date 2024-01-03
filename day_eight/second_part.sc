// Advent of Code Day 8
// Adam Gluck

type NodePath = (String, String)
type NodeMap = Map[String, NodePath]

// Path length of A -> Z and Z -> Z seem to be the same in input
def getCycleLen(
    node: String,
    nodeMap: NodeMap,
    instructions: List[Char]
): Long = {
  var step = 0
  var curNode = node

  // Get length of path from A -> Z
  while (!curNode.endsWith("Z")) {
    var index = step % instructions.length
    instructions(index) match
      case 'L' => curNode = nodeMap.getOrElse(curNode, (curNode, curNode))._1
      case 'R' => curNode = nodeMap.getOrElse(curNode, (curNode, curNode))._2
    step += 1
  }
  step.toLong
}

@main
def dayEight() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  //Process input into data structures
  var instructions: List[Char] = List()
  var nodeMap: Map[String, (String, String)] = Map()
  content.split("\n\n") match
    case Array(instructStr, nodeStr) => {
      instructions = instructStr.toCharArray.toList

      nodeMap = nodeStr
        .split("\n")
        .collect({ case s"$node = ($lPath, $rPath)" =>
          (node, (lPath, rPath))
        })
        .foldLeft(nodeMap: Map[String, (String, String)])(
          (acc, x: (String, (String, String))) =>
            (acc + (x._1 -> (x._2._1, x._2._2)))
        )

    }
    case _ =>
      throw new IllegalArgumentException("Invalid input format")

  // Get cycle lengths for each node
  var curNodes = nodeMap.keys.filter(_.endsWith("A")).toList

  // Find LCM of all paths
  def gcd(a: Long, b: Long): Long = if (b == 0) a.abs else gcd(b, a % b)
  def lcm(a: Long, b: Long): Long = (a * b).abs / gcd(a, b)
  curNodes
    .map(getCycleLen(_, nodeMap, instructions))
    .reduce((a, b) => lcm(a, b))
}

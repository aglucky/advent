// Advent of Code Day 8
// Adam Gluck

type NodePath = (String, String)
type NodeMap = Map[String, NodePath]

@main
def dayEight() = {
  // Read in file for input
  val input_path: os.Path = os.pwd / "input.txt"
  val content: String = os.read(input_path)

  // Process input into data structures
  var instructions: List[Char] = List()
  var nodeMap: NodeMap = Map()
  content.split("\n\n") match
    case Array(instructStr, nodeStr) => {
      instructions = instructStr.toCharArray.toList

      nodeMap = nodeStr
        .split("\n")
        .collect({ case s"$node = ($lPath, $rPath)" =>
          (node, (lPath, rPath))
        })
        .foldLeft(nodeMap: NodeMap)((acc, x: (String, NodePath)) =>
          (acc + (x._1 -> (x._2._1, x._2._2)))
        )

    }
    case _ =>
      throw new IllegalArgumentException("Invalid input format")

  // Find length of Path A -> Z
  var curNode: String = "AAA"
  var step = 0
  while (curNode != "ZZZ") {
    var index = step % instructions.length
    instructions(index) match
      case 'L' => curNode = nodeMap.getOrElse(curNode, (curNode, curNode))._1
      case 'R' => curNode = nodeMap.getOrElse(curNode, (curNode, curNode))._2
    step += 1
  }

  step
}

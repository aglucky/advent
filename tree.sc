@main
def print_tree(size: Int = 10, stumpLen: Int = 3, level: Int = 0): Unit = {
  size match
    // Create tree with recursion
    case size if size > level => {
      val spaces = " " * (size - level)
      val stars = "*" * (1 + 2 * level)
      println(s"$spaces$stars$spaces")
      print_tree(size, stumpLen, level + 1)
    }
    // Create stump as base case
    case _ => {
      (0 until stumpLen).foreach((i) => {
        val spaces = " " * (size)
        println(s"$spaces*$spaces")
      })
    }
}

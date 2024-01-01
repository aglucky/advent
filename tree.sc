@main
def print_tree(size: Int, level: Int = 0): Unit = {
  size match
    case size if size > level => {
      val spaces = " " * (size - level)
      val stars = "*" * (1 + 2 * level)
      println(s"$spaces$stars$spaces")
      print_tree(size, level + 1)
    }
    case _ => println
}

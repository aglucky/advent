@main
def print_tree(size: Int, level: Int=0): Unit = {
    if (level == size)
        return
    else
        val spaces = " " * (size - level)
        val stars = "*" * (1 + 2 * level)
        println(s"$spaces$stars$spaces")
        print_tree(size, level + 1)
}
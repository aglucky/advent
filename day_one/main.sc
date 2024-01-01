// Advent of Code Day 1
// Adam Gluck

def getNums(line: String): List[Int] = {
    // Set input to lowercase for consistent matching in Map
    val lowercase_line = line.toLowerCase
    val word_nums: Map[String, Int] = Map(
        "one" -> 1, "two" -> 2, "three" -> 3, "four" -> 4, 
        "five" -> 5, "six" -> 6, "seven" -> 7, "eight" -> 8, 
        "nine" -> 9, "zero" -> 0
    )

    //Convert each (letter, index in word) tuple of line to 0 or 1 numbers
    val line_numbers: List[Int] = lowercase_line.zipWithIndex.flatMap { case (letter, index) =>
        if (letter.isLetter) {
            // Check if letter is start of num_word, will return None if no matches
            word_nums.collectFirst {
                case (word, num) if lowercase_line.startsWith(word, index) => num //Add num if letter is start of num's word form
            }
        } else if (letter.isDigit) {
            Some(letter.asDigit) // Add char as digit if it is digit
        } else {
            None // Add nothing if no match
        }
    }.toList

    line_numbers //Return statement is optional in scala
}

@main
def dayOne(): Int = {
    // Read in file for input
    val input_path: os.Path = os.pwd / "input.txt"
    val content: String = os.read(input_path)
    
    // Convert input to solution
    val sum: Int = content
                    .split("\n") //Get lines in input files
                    .map(getNums) //Convert to numbers
                    .filter(_.nonEmpty) //Don't count line if empty
                    .map(x => if (x.length == 1) s"${x.head}${x.head}" else s"${x.head}${x.last}" ) //Convert to 2 digit number with first and last in list
                    .map(_.toInt)
                    .reduce((x,y)=>x+y) //Sum all lines
                
    sum
}


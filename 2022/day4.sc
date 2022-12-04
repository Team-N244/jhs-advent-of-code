import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val lines = readFile("day4-input.txt")
val fmtRegex = "(\\d+)-(\\d+),(\\d+)-(\\d+)".r

val ranges = lines.map { l =>
  val m = fmtRegex.findFirstMatchIn(l).get // Yay no error handling
  val range1 = m.group(1).toInt to m.group(2).toInt
  val range2 = m.group(3).toInt to m.group(4).toInt
  range1 -> range2
}

val part1 = ranges.count { case (range1, range2) =>
  range1.containsSlice(range2) || range2.containsSlice(range1)
}
println(part1)

val part2 = ranges.count { case (range1, range2) =>
  (range1.last >= range2.head && range1.last <= range2.last) ||
  (range2.last >= range1.head && range2.last <= range1.last)
}
println(part2)

val part1Improved = ranges.count { case (range1, range2) =>
  (range1.head >= range2.head && range1.last <= range2.last) ||
  (range2.head >= range1.head && range2.last <= range1.last)
}
println(part1Improved)

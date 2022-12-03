//> using scala "2"

import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val lines = readFile("day3-input.txt")

def priority(c: Char) = if (c.isUpper) c.toInt - 38 else c.toInt - 96

// Part 1
val diffs = lines.map { l =>
  val firstHalf: Seq[Char] = l.substring(0, l.length / 2)
  val firstSet = firstHalf.toSet
  val secondHalf: Seq[Char] = l.substring(l.length / 2)
  val secondSet = secondHalf.toSet
  firstSet.intersect(secondSet).head 
}

val priorities = diffs.map(priority)

println(priorities.sum)


// Part 2
val groups = lines.sliding(3, 3)
val badges = groups.map { g => 
  g.reduce((o,t) => o.intersect(t)).head
}

println(badges.map(priority).sum)



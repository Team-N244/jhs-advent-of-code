//> using scala "2"

import scala.io.Source

val sortedCalories = Source.fromFile("day1-input.txt")
  .getLines
  .foldLeft((0, Seq.empty[Int])){ case ((current, calories), l) =>
    if (l.trim.isEmpty) {
      0 -> (calories :+ current)
    } else {
      (current + l.trim.toInt) -> calories
    }
  }
  ._2
  .sorted(Ordering.Int.reverse)

println(sortedCalories.head)
println(sortedCalories.take(3).sum)

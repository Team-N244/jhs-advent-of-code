//> using scala "2"

import scala.io.Source
import scala.collection.mutable.ListBuffer

var largest = 0
var current = 0

// Part 1
for (l <- Source.fromFile("day1-input.txt").getLines) {
  if (l.trim.isEmpty) {
    largest = Math.max(largest, current)
    current = 0
  } else {
    current += l.trim.toInt
  }
}

println(largest)

// Part 2
current = 0
var all = new ListBuffer[Int]()
for (l <- Source.fromFile("day1-input.txt").getLines) {
  if (l.trim.isEmpty) {
    all += current
    current = 0
  } else {
    current += l.trim.toInt
  }
}

println(all.toList.sorted(Ordering.Int.reverse).take(3).sum)

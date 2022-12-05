//> using scala "3"

import scala.io.Source
import scala.collection.mutable.Stack

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val lines = readFile("day5-input.txt")

// Part 1

// Init and fill stacks
val (instructionLines, initLines) = lines.partition(_.startsWith("move"))
val stackLines = initLines.reverse.drop(2)
val stacks = initLines
  .flatMap(_.zipWithIndex.filter(t => t._1.isLetter))
  .groupMap(_._2)(_._1)
  .toList
  .sortBy(_._1)
  .map(_._2.reverse)
  .map(_.foldLeft(Stack[Char]())((s, c) => s.push(c)))
  .toArray

// Move stuff
val instructorRegex = "move (\\d+) from (\\d+) to (\\d+)".r
instructionLines.foreach { i =>
  val m = instructorRegex.findFirstMatchIn(i).get
  val count = m.group(1).toInt
  val from = m.group(2).toInt - 1
  val to = m.group(3).toInt - 1
  val crates = (0 until count).map(_ => stacks(from).pop)
  //stacks(to).pushAll(crates)         // Part 1
  stacks(to).pushAll(crates.reverse) // Part 2
}
println(stacks.map(_.head).mkString(""))



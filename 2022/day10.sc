//> using scala "3"

import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val lines = readFile("day10-input.txt")
var x = 1

val frames = lines.flatMap {
  case "noop" => Seq(x)
  case l =>
    val inc = l.substring(5).trim.toInt
    val old = x
    x += inc
    Seq(old, old)
}.toArray


println((20 to frames.length by 40).map(i => i * frames(i - 1)).sum)

val output = frames.sliding(40, 40)
  .map(_.zipWithIndex.map(x => if (x._2 >= x._1 - 1 && x._2 <= x._1 + 1) "#" else ".").mkString)
  .mkString("\n")
println(output)

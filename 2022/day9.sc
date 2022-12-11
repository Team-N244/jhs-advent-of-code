//> using scala "3"

import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val lines = readFile("day9-input.txt")

class Rope(x: Int, y: Int):
  private var h: (Int, Int) = x -> y
  private var t: (Int, Int) = x -> y

  def moveHelper(dx: Int, dy: Int): (Int, Int) = 
    val oldH = h
    h = (h._1 + dx) -> (h._2 + dy)
    if (h._1 - 2 == t._1 || h._1 + 2 == t._1 || h._2 - 2 == t._2 || h._2 + 2 == t._2)
      println(s"$h is now too far from $t, moving to $oldH")
      t = oldH
    t

  def move(d: String, cnt: Int): Seq[(Int, Int)] = 
    println(s"$d $cnt")
    d match 
      case "U" => (0 until cnt).map(_ => moveHelper(0, 1))
      case "D" => (0 until cnt).map(_ => moveHelper(0, -1))
      case "L" => (0 until cnt).map(_ => moveHelper(-1, 0))
      case "R" => (0 until cnt).map(_ => moveHelper(1, 0))
    

val rope = new Rope(0, 0)
val tailLocs = lines.flatMap { line =>
  val dir = line.substring(0, 1)
  val times = line.substring(1).trim.toInt
  rope.move(dir, times)
}.distinct
println(tailLocs.size)

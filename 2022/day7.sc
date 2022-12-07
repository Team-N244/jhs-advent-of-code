//> using scala "3"

import scala.io.Source
import scala.collection.mutable.ListBuffer

def readFile(f: String): Iterator[String] = Source.fromFile(f).getLines

val lines = readFile("day7-input.txt")

// Build the tree
sealed trait Node:
  val name: String
  def size: Long
  def flatten: Seq[Node]

case class Directory(name: String, var n: Seq[Node], parent: Directory) extends Node:
  def size: Long = n.map(_.size).sum
  def flatten: Seq[Node] = this +: n.flatMap(_.flatten)

case class File(name: String, size: Long) extends Node:
  def flatten: Seq[Node] = Seq(this)

var fileSystem: Node = _
var current: Directory = _
var line: String = lines.next()

val cdPattern = "\\$ cd (.+)".r
val lsPattern = "\\$ ls".r
while (line != null) do
  line match
    case cdPattern(d) => 
      if current == null then
        val node = Directory(d, Nil, null)
        fileSystem = node
        current = node
      else if d == ".." then
        current = current.parent
      else
        current = current.n.find(_.name == d).get.asInstanceOf[Directory]
      line = if (lines.hasNext) lines.next else "END"
    case lsPattern => 
      line = lines.next()
      var directory = ListBuffer[Node]()
      while (line != null && !line.startsWith("$")) do
        val parts = line.split(" ")
        if (parts.head == "dir") then
          directory += Directory(parts.last, Nil, current)
        else
          directory += File(parts.last, parts.head.toLong)
        line = if (lines.hasNext) lines.next else null
      current.n = directory.toSeq

val part1 = fileSystem
  .flatten
  .filter(d => d.isInstanceOf[Directory] && d.size < 100000)
  .map(_.size)
  .sum
  
println(part1)
println()

val totalSize = fileSystem.size
val unusedSpace = 70000000 - totalSize
val freeTarget = 30000000 - unusedSpace
val part2 = fileSystem
  .flatten
  .filter(d => d.isInstanceOf[Directory] && d.size >= freeTarget)
  .map(_.size)
  .sorted
  .head
println(part2)

//> using scala "3"

import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val input = readFile("day6-input.txt").head

def findfirstUnique(input: String, s: Int): Int = 
  input.sliding(s)
    .map(_.distinct)
    .zipWithIndex
    .find(_._1.length == s)
    .get
    ._2 + s

def findFirstUniqueEfficient(input: String, s:Int): Int =
  val sub = input.substring(0, s)
  if (sub.distinct.length == s)
    s
  else
    1 + findFirstUniqueEfficient(input.substring(1), s)

println(findfirstUnique(input, 4))
println(findfirstUnique(input, 14))
println(findFirstUniqueEfficient(input, 4))
println(findFirstUniqueEfficient(input, 14))

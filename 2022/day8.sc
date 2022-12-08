import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

val lines = readFile("day8-input.txt")

val treeMap = lines.map(_.map(_.toInt).toArray).toArray

def isVisible(map: Array[Array[Int]], r: Int, c: Int): Boolean = {
  val height = map(r)(c)
  r == 0 || r == map.length - 1 || c == 0 || c == map.head.length ||
    (r + 1 until map.length).forall(rr => map(rr)(c) < height) ||
      (0 until r).forall(rr => map(rr)(c) < height) ||
      (0 until c).forall(cc => map(r)(cc) < height) ||
      (c+1 until  map.head.length).forall(cc => map(r)(cc) < height)
}


// Part 1
val countPerRow = treeMap.indices.map { row =>
  treeMap.head.indices.count(column => isVisible(treeMap, row, column))
}

println(countPerRow.sum)

// Part 2
def viewDistance(map: Array[Array[Int]], r: Int, c: Int): Int = {
  val height = map(r)(c)
  val southRange = r + 1 until map.length
  val south = southRange.takeWhile(rr => map(rr)(c) < height).length + 1

  val northRange = r -1 to 0 by -1
  val north = northRange.takeWhile(rr => map(rr)(c) < height).length + 1

  val westRange = c -1 to 0 by -1
  val west = westRange.takeWhile(cc => map(r)(cc) < height).length + 1
  
  val eastRange = c+1 until  map.head.length
  val east = eastRange.takeWhile(cc => map(r)(cc) < height).length + 1

  Math.min(south, southRange.length) *
    Math.min(north, northRange.length) *
    Math.min(west, westRange.length) *
    Math.min(east, eastRange.length)
}

val highestScore = treeMap.indices.map { row =>
  treeMap.head.indices.map(column => viewDistance(treeMap, row, column)).max
}.max

println(highestScore)

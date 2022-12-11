// Tried to solve part 2 and got stuck so this isnt right anymore
//
//
//> using scala "3"

import scala.io.Source

def readFile(f: String): Seq[String] = Source.fromFile(f).getLines.toSeq

class Monkey(starting: Seq[Long], op: Long => Long, test: (Long, Long) => (Long, Int)):
  private var items = starting
  private var seen = 0

  def itemsSeen: Int = seen

  def round(divisor: Long): Seq[(Long, Int)] =
    val results = items
      .map(op)
      .map(test(_, divisor))
    seen += results.length
    items = Nil
    results

  def add(item: Long): Unit = items = items :+ item

var divisor = 1L

val monkeys = readFile("day11-input.txt")
  .filterNot(_.trim.isEmpty)
  .sliding(6, 6)
  .map(_.toArray)
  .map { lines =>
    val starting = lines(1).replaceAll("Starting items:", "").split(",").map(_.trim.toLong).toSeq
    val opparsed = lines(2).replaceAll("Operation: new = old ", "").trim.split("\\s+")
    val op = opparsed.head match
      case "+" if opparsed.last == "old" => (old: Long) => old + old
      case "+" => (old: Long) => old + opparsed.last.toLong
      case "-" => (old: Long) => old - opparsed.last.toLong
      case "*" if opparsed.last== "old" => (old: Long) => old * old
      case "*" => (old: Long) => old * opparsed.last.toLong
      case "/" => (old: Long) => old / opparsed.last.toLong

    val testValue = lines(3).replaceAll("Test: divisible by", "").trim.toInt
    divisor *= testValue
    val trueMonkey = lines(4).replaceAll("If true: throw to monkey", "").trim.toInt
    val falseMonkey = lines(5).replaceAll("If false: throw to monkey", "").trim.toInt
    val test = (input: Long, divisor: Long) => {
      if input % testValue == 0 then input % divisor -> trueMonkey else input % divisor -> falseMonkey
    }
    new Monkey(starting, op, test)
  }
  .toArray

for (i <- 0 until 10000) {
  for (m <- monkeys.indices) {
    val items = monkeys(m).round(divisor)
    items.foreach { case (item, monkey) =>
      monkeys(monkey).add(item)
    }
  }
}

val itemsSeen = monkeys.map(_.itemsSeen).sorted.reverse
println(itemsSeen.head.toLong * itemsSeen(1).toLong)

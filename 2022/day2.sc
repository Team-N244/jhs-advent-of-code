//> using scala "2"

import scala.io.Source

sealed trait Action {
  val points: Int
}
case object Rock extends Action {
  val points = 1
}
case object Paper extends Action {
  val points = 2
}
case object Scissors extends Action {
  val points = 3
}

sealed trait Result {
  val points: Int 
}

case object Win extends Result {
  val points = 6
}
case object Draw extends Result {
  val points = 3
}
case object Loss extends Result {
  val points = 0
}

def round(opp: Action, you: Action): Int = {
  val res = (opp, you) match {
    case (o, y) if o == y => Draw
    case (Rock, Scissors) => Loss
    case (Paper, Rock) => Loss
    case (Scissors, Paper) => Loss
    case _ => Win
  }

  res.points + you.points
}

val oppMap = Map('A' -> Rock, 'B' -> Paper, 'C' -> Scissors)
val youMap = Map('X' -> Rock, 'Y' -> Paper, 'Z' -> Scissors)

val input = Source.fromFile("day2-input.txt").getLines
val rounds = input.map { l =>
  val opp = oppMap(l.charAt(0))
  val you = youMap(l.charAt(2))
  round(opp, you)
}
println(rounds.sum)


// Part 2
val youResultMap = Map('X' -> Loss, 'Y' -> Draw, 'Z' -> Win)
val rounds2 = input.map { l =>
  val opp = oppMap(l.charAt(0))
  val res = youResultMap(l.charAt(2))
  val you = (res, opp) match {
    case (Draw, o) => o
    case (Win, Rock) => Paper
    case (Win, Paper) => Scissors
    case (Win, Scissors) => Rock
    case (Loss, Rock) => Scissors
    case (Loss, Paper) => Rock
    case (Loss, Scissors) => Paper
  }
  res.points + you.points

}
println(rounds2.sum)



package cs4r.labs.learningscala.adventofcode

import scala.annotation.tailrec
import scala.collection.immutable.::


object AdventOfCode10 extends App {

  val puzzleInput =
    """3113322113""".stripMargin

  val listOfInputs: List[List[Int]] = puzzleInput.split("\n").map(e => e.toList.map(b => b.toString.toInt)).toList

  def lookAndSay(list: List[Int]): List[Int] = {
    @tailrec
    def lookAndSayTailRec(pending: List[Int], computed: List[Int]) : List[Int] = pending match {
        case Nil => computed
        case l => l match {
          case h :: Nil => lookAndSayTailRec(List(), computed ++ List(1, h))
          case h :: t => {
            val (repetitions: List[Int], remaining: List[Int]) = l.span(_ == h)
            lookAndSayTailRec(remaining, computed ++ List(repetitions.size, h))
          }
          case _ => List()
        }
    }
    lookAndSayTailRec(list, List())
  }

  @tailrec
  def applyNTimes[E](f: E => E)(n: Int)(seed: E): E = n match {
    case n if n == 0 => seed
    case _ => {
      println(s"iteration number: $n")
      val fx:E = f(seed); applyNTimes(f)(n - 1)(fx)
    }
  }

  println(listOfInputs)


  //println(applyNTimes(lookAndSay _, 40)(initial))

  println(applyNTimes(lookAndSay _)(50)(listOfInputs.head).size)

}

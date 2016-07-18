package cs4r.labs.learningscala.adventofcode
import scala.annotation.tailrec

object AdventOfCode11 extends App {

  val puzzleInput = """cqjxjnds""".stripMargin
  val inputAsList = puzzleInput.split("\n").map(_.toList).toList.head

  def incrChar(c: Char): (Char, Boolean) = {
    if (c + 1 <= 'z') ((c + 1).toChar, false)
    else ('a', true)
  }

  def increaseSequence(s: List[Char]): List[Char] = {
    val (char, remainder) = incrChar(s.last)
    if (!remainder) {
      s.init ::: List(char)
    } else {
      increaseSequence(s.init) ::: List(char)
    }
  }

  def firstRequierement(l: List[Char]) = {
    l.zip(l.drop(1)).zip(l.drop(2)).map(t => (t._1._1, t._1._2, t._2)).exists(t => t._1 + 1 == t._2 && t._2 + 1 == t._3)
  }

  def secondRequirement(l: List[Char]) = {
    !l.exists(e => e == 'i' || e == 'o' || e == 'l')
  }

  def thirdRequirement(l: List[Char]) = {
    l.zip(l.drop(1)).filter(t => t._1 == t._2).distinct.size >= 2
  }

  def and[A](ps: (A => Boolean)*) = (a: A) => ps.forall(_(a))

  @tailrec
  def applyUntil[E](f: E => E)(p: E => Boolean)(e: E): E = {
    if (p(e)) e
    else applyUntil(f)(p)(f(e))
  }

  val partARequirements = and(firstRequierement _, secondRequirement _, thirdRequirement _)

  val partAResult = applyUntil(increaseSequence _)(partARequirements)(inputAsList)

  println(partAResult.map(_.toString).reduce(_ + _))

  val usedPasswords = List(inputAsList, partAResult)

  val partBRequirements = and(partARequirements, (l: List[Char]) => !usedPasswords.contains(l))

  val partBResult = applyUntil(increaseSequence _)(partBRequirements)(inputAsList)

  println(partBResult.map(_.toString).reduce(_ + _))
}
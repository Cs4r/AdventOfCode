package cs4r.labs.learningscala.adventofcode

import scala.annotation.tailrec


object AdventOfCode25 extends App {


  val puzzleInput = (3010, 3019)
  val row = puzzleInput._1-1
  val column = puzzleInput._2-1


  val initialNumbers = Array(
    Array(20151125, 18749137, 17289845, 30943339, 10071777, 33511524),
    Array(31916031, 21629792, 16929656, 7726640, 15514188, 4041754),
    Array(16080970, 8057251, 1601130, 7981243, 11661866, 16474243),
    Array(24592653, 32451966, 21345942, 9380097, 10600672, 31527494),
    Array(77061, 17552253, 28094349, 6899651, 9250759, 31663883),
    Array(33071741, 6796745, 25397450, 24659492, 1534922, 27995004)
  )


  val numberOfStepsToComputeCode : ((Int, Int) => Long) = (i, j) => {

    @tailrec
    def stepsToStartFromARow(i: Int, j: Int, acc: Long): Long = {
      if (j == 0) acc
      else stepsToStartFromARow(i + 1, j - 1, acc + 1)
    }

    @tailrec
    def minMatrixSizeToComputeCode(i: Int, j: Int): Long = {
      if (j == 0) i + 1
      else minMatrixSizeToComputeCode(i + 1, j - 1)
    }

    val toBeginningOfARow = stepsToStartFromARow(i, j, 1)
    val minMatrixSize = minMatrixSizeToComputeCode(i, j)

    ( ((minMatrixSize - 1) * minMatrixSize) / 2 + toBeginningOfARow )-1 // -1 is because we always begin with a seed
  }

  val computeCode : ((Array[Array[Int]], Int, Int) => Long ) =  (input, i,j ) => {
    @tailrec
    def applyNTimes[E](f: E => E)(n: Long)(seed: E): E = n match {
      case n if n == 0 => seed
      case _ =>
        val fx:E = f(seed); applyNTimes(f)(n - 1)(fx)

    }

    if(input.isDefinedAt(i) && input(i).isDefinedAt(j)) input(i)(j)
    else {
      val seed = input(0)(input.length-1)
      val steps: Long = numberOfStepsToComputeCode(i,j) - numberOfStepsToComputeCode(0, input.length-1)
      applyNTimes((e:Long) => (e * 252533) % 33554393)(steps)(seed)
    }
  }

  var partA = computeCode(initialNumbers, row, column)
  println(partA)

}

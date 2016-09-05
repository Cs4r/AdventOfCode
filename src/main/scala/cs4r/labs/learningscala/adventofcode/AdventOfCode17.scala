package cs4r.labs.learningscala.adventofcode

object AdventOfCode17 extends App {

  val puzzleInput =
    """50
      |44
      |11
      |49
      |42
      |46
      |18
      |32
      |26
      |40
      |21
      |7
      |18
      |43
      |10
      |47
      |36
      |24
      |22
      |40""".stripMargin

  val containers = puzzleInput.split("\n").map(_.toInt).toList

  val limit = math.pow(2, containers.size).toInt

  val partAQuantity = 150

  val allCombinations = (0 until limit).map(i => String.format("%" + containers.size + "s", Integer.toBinaryString(i)).replace(' ', '0')).map(_.map(_.toInt - 48).toList)

  val validEggnogCombinations =  allCombinations.map(_.zip(containers)).map(z => ( z.map({ case (1, b) => 1 case (a,b) => 0 }).sum, z.map({ case (a, b) => a * b }).sum)).filter(_._2 == partAQuantity)

  val partA = validEggnogCombinations.size

  println(partA)

  val mininumNumberOfContainersUsed = validEggnogCombinations.minBy(_._1)._1

  println(mininumNumberOfContainersUsed)

  val partB = validEggnogCombinations.count(_._1 == mininumNumberOfContainersUsed)

  print(partB)

}

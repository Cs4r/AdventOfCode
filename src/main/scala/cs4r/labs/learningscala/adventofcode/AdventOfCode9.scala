package cs4r.labs.learningscala.adventofcode

object AdventOfCode9 extends App {


  val puzzleInput =
    """Tristram to AlphaCentauri = 34
      |Tristram to Snowdin = 100
      |Tristram to Tambi = 63
      |Tristram to Faerun = 108
      |Tristram to Norrath = 111
      |Tristram to Straylight = 89
      |Tristram to Arbre = 132
      |AlphaCentauri to Snowdin = 4
      |AlphaCentauri to Tambi = 79
      |AlphaCentauri to Faerun = 44
      |AlphaCentauri to Norrath = 147
      |AlphaCentauri to Straylight = 133
      |AlphaCentauri to Arbre = 74
      |Snowdin to Tambi = 105
      |Snowdin to Faerun = 95
      |Snowdin to Norrath = 48
      |Snowdin to Straylight = 88
      |Snowdin to Arbre = 7
      |Tambi to Faerun = 68
      |Tambi to Norrath = 134
      |Tambi to Straylight = 107
      |Tambi to Arbre = 40
      |Faerun to Norrath = 11
      |Faerun to Straylight = 66
      |Faerun to Arbre = 144
      |Norrath to Straylight = 115
      |Norrath to Arbre = 135
      |Straylight to Arbre = 127""".stripMargin


  val testInput =
    """London to Dublin = 464
      |London to Belfast = 518
      |Dublin to Belfast = 141""".stripMargin


  val distanceMatrix: Map[(String, String), Int] = puzzleInput.split("\n").toList.foldLeft(Map[(String, String), Int]())((map, current) => {
    val linePattern = "([A-Za-z ]+)\\s+to\\s+([A-Za-z ]+)\\s+=\\s+([0-9]+)".r
    val linePattern(cityA, cityB, distance) = current
    map + ((cityA, cityB) -> distance.toInt) + ((cityB, cityA) -> distance.toInt)
  })

  val cities: Set[String] = distanceMatrix.keys.flatMap(t => Set(t._1, t._2)).toSet

  println(s"Number of cities: ${cities.size}")

  def greedy(chosen: List[String], currentCity: String, currentDistance: Int, citiesLeft: Set[String])(costFunction: (Int, Int) => Int): (Int, List[String]) = citiesLeft.toList match {
    case h :: Nil => {
      (currentDistance + distanceMatrix.get((currentCity, h)).get, chosen :+ h)
    }
    case _ => {
      val adjacentCitiesGroupByDistance = distanceMatrix.keys.filter(x => x._1 == currentCity && !chosen.contains(x._2)).map(t => (t._2, distanceMatrix.get(t).get)).groupBy(x => x._2)
      val betterDistance: Int = adjacentCitiesGroupByDistance.keys.reduceOption(costFunction).get
      val closestCity: String = adjacentCitiesGroupByDistance.get(betterDistance).get.head._1
      greedy(chosen :+ closestCity, closestCity, currentDistance + betterDistance, citiesLeft - closestCity)(costFunction)
    }
  }


  def minPath(from: String, cities: Set[String], distanceMatrix: Map[(String, String), Int]) = {
    greedy(List(from), from, 0, (cities - from))(Math.min)
  }

  def maxPath(from: String, cities: Set[String], distanceMatrix: Map[(String, String), Int]) = {
    greedy(List(from), from, 0, (cities - from))(Math.max)
  }

  println(cities.map(minPath(_, cities, distanceMatrix)).reduce((path1, path2) => {
    if (path1._1 < path2._1) path1 else path2
  }))

  println(cities.map(maxPath(_, cities, distanceMatrix)).reduce((path1, path2) => {
    if (path1._1 > path2._1) path1 else path2
  }))

}

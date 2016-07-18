package cs4r.labs.learningscala.adventofcode

import scala.annotation.tailrec

object AdventOfCode14 extends App {

  val puzzleInput =
    """Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
      |Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
      |Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
      |Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
      |Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
      |Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
      |Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
      |Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
      |Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.""".stripMargin

  case class ReindeerInfo(val name: String, val speed: Int, val workingTime: Int, val restingTime: Int)

  val linePattern = "(\\p{Alpha}+)[\\p{Alpha}\\p{Space}]*([0-9]+) km/s[\\p{Alpha}\\p{Space}]*([0-9]+) seconds,[\\p{Alpha}\\p{Space}]*([0-9]+) seconds\\.".r

  val linePattern(name, km, time, resting) = "Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds."


  val listOfReindeerInfo = puzzleInput.split("\n").map(l => {
    val linePattern(name, speed, working, resting) = l.trim
    ReindeerInfo(name, speed.toInt, working.toInt, resting.toInt)
  })

  def distanceAtSecond(reindeerInfo: ReindeerInfo)(second: Int): Int = {

    @tailrec
    def distanceAtSecondTailRec(reindeerInfo: ReindeerInfo, second: Int, currentDistance: Int): Int = {
      if (second <= 0) {
        currentDistance
      } else if (second <= reindeerInfo.workingTime) {
        distanceAtSecondTailRec(reindeerInfo, 0, currentDistance + reindeerInfo.speed * second)
      } else {
        val remaining = second - reindeerInfo.workingTime - reindeerInfo.restingTime
        distanceAtSecondTailRec(reindeerInfo, remaining, currentDistance + (reindeerInfo.speed * reindeerInfo.workingTime))
      }
    }
    distanceAtSecondTailRec(reindeerInfo, second, 0)
  }

  val second = 2503

  val partAResult = listOfReindeerInfo.map(distanceAtSecond(_)(second)).max

  println(partAResult)

  val pointsPerReindeer = (1 to second).flatMap(s => {
    val sortedByLeading = listOfReindeerInfo.map(reindeerInfo => (reindeerInfo, distanceAtSecond(reindeerInfo)(s))).sortWith(_._2 > _._2)
    sortedByLeading.takeWhile(_._2 == sortedByLeading.head._2).map(w => (w._1.name, 1)) // one point to each winner
  }).groupBy(_._1).mapValues(_.size)

  val partBResult = pointsPerReindeer.maxBy(_._2)

  println(partBResult._2)

}
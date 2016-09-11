package cs4r.labs.learningscala.adventofcode

import scala.collection.mutable.ListBuffer


object AdventOfCode24 extends App {

  val puzzleInput =
    """1
      |3
      |5
      |11
      |13
      |17
      |19
      |23
      |29
      |31
      |41
      |43
      |47
      |53
      |59
      |61
      |67
      |71
      |73
      |79
      |83
      |89
      |97
      |101
      |103
      |107
      |109
      |113""".stripMargin

  val packagesInput =  puzzleInput.split("\n").map(_.toInt).toList.sorted

  println(packagesInput)

  val threeGroups: (List[Int] => (List[Int], List[Int], List[Int])) = (packages) => {

    val configurations = ListBuffer[(List[Int], List[Int], List[Int])]()

    for (i <- 1 until packages.size; group1 <- packages.combinations(i) if configurations.isEmpty) {

      val left = packages.filterNot(group1.contains(_))

      if (group1.sum >= (left.sum / 2) && left.min*left.size/2 <= group1.sum) {

        for (j <- 1 until left.size; group2 <- left.combinations(j).filter(group1.sum == _.sum) if group2.size <= left.size / 2 && configurations.isEmpty) {

          val group3 = packages.filterNot(e => group1.contains(e) || group2.contains(e))

          if (group2.sum == group3.sum) {
            val configuration: (List[Int], List[Int], List[Int]) = (group1, group2, group3)
            configurations += configuration
          }
        }
      }
    }

    configurations.head
  }


  val partA = threeGroups(packagesInput)._1.map(_.toLong).product

  println(partA)


  // I am not proud of this solution, but what the heck, it works!

  val fourGroups: (List[Int] => (List[Int], List[Int], List[Int], List[Int])) = (packages) => {

    val configurations = ListBuffer[(List[Int], List[Int], List[Int], List[Int])]()

    for (i <- 1 until packages.size; group1 <- packages.combinations(i) if configurations.isEmpty) {

      val left1 = packages.filterNot(group1.contains(_))

      if (group1.sum >= (left1.sum / 3) && left1.min * left1.size / 3 <= group1.sum) {

        for (j <- 1 until left1.size; group2 <- left1.combinations(j).filter(group1.sum == _.sum) if group2.size <= left1.size / 3 && configurations.isEmpty) {

          val left2 = left1.filterNot(e => group2.contains(e))

          if (group2.sum >= (left2.sum / 2) && left2.min * left2.size / 2 <= group2.sum) {

            for (k <- 1 until left2.size; group3 <- left2.combinations(k).filter(group2.sum == _.sum) if group3.size <= left2.size / 2 && configurations.isEmpty) {

              val group4 = left2.filterNot(e => group3.contains(e))

              if (group3.sum == group4.sum) {
                val configuration: (List[Int], List[Int], List[Int], List[Int]) = (group1, group2, group3, group4)
                println(configuration)
                configurations += configuration
              }
            }
          }
        }
      }
    }

    configurations.head
  }

  val partB = fourGroups(packagesInput)._1.map(_.toLong).product

  println(partB)

}

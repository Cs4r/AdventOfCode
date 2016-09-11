package cs4r.labs.learningscala.adventofcode

import scala.annotation.tailrec

object AdventOfCode21 extends App {

  trait Player {
    def points: Int

    def damage: Int

    def armor: Int
  }

  case class Boss(points: Int, damage: Int, armor: Int) extends Player

  case class Me(points: Int, features: List[Feature]) extends Player{

    def gold : Int = features.distinct.map(_.cost).sum

    def armor : Int = features.distinct.map(_.armor).sum

    def damage : Int = features.distinct.map(_.damage).sum
  }

  trait Feature {
    def cost: Int

    def damage: Int

    def armor: Int
  }

  case class Weapon(cost: Int, damage: Int) extends Feature {
    def armor = 0
  }

  case class Armor(cost: Int, armor: Int) extends Feature {
    def damage = 0
  }

  case class Ring(cost: Int, damage: Int, armor: Int) extends Feature

  val canWinBattle: (Me, Boss) => Boolean = (me, boss) => {

    val myDamage = attakcerDamage(me, boss)
    val bossDamage = attakcerDamage(boss, me)

    @tailrec
    def canWin(myPoints: Int, bossPoints: Int): Boolean = {
      if (bossPoints - myDamage <= 0) {
        true
      }
      else if (myPoints - bossDamage <= 0) false
      else canWin(myPoints - bossDamage, bossPoints - myDamage)
    }

    canWin(me.points, boss.points)
  }

  val attakcerDamage: (Player, Player) => Int = (attacker, defender) => if (defender.armor >= attacker.damage) 1 else attacker.damage - defender.armor

  val weapons = List(Weapon(8,4),
                    Weapon(10,5),
                    Weapon(25,6),
                    Weapon(40,7),
                    Weapon(74,8))

  val armors = Armor(0,0) :: List(Armor(13,1),
                  Armor(31,2),
                  Armor(53,3),
                  Armor(75,4),
                  Armor(102,5))

  val rings =   Ring(0,0,0)::  Ring(0,0,0):: List(Ring(25,1,0),
                  Ring(50,2,0),
                  Ring(100,3,0),
                  Ring(20,0,1),
                  Ring(40,0,2),
                  Ring(80,0,3) )

  val boss = Boss(104, 8, 1)

  val ringsOrdered = rings.combinations(2).toList.sortWith((a, b) => a.map(_.cost).sum < b.map(_.cost).sum)

  var combinations: List[Me] = for { weapon <- weapons; armor <- armors; ringsToUse <- ringsOrdered}  yield Me(100, weapon :: armor :: ringsToUse)

  var partA = combinations.filter(canWinBattle(_, boss)).minBy(_.gold)

  println(partA.gold)

  var partB = combinations.filterNot(canWinBattle(_, boss)).maxBy(_.gold)

  println(partB.gold)
}

package cs4r.labs.learningscala.adventofcode

import java.security.MessageDigest

object AdventOfCode4 extends App {

  val puzzleInput: String = "abcdef"
  val input: String = puzzleInput + 609043

  def md5(s: String) = {
    val instance: MessageDigest = MessageDigest.getInstance("MD5")
    instance.digest(s.getBytes)
  }

  def startWithFiveZeros(s: Array[Byte]) = {
    s.take(5).forall(_ == 0x00)
  }

  def printHex(array: Array[Byte]) = {
    print("0x")
    array.foreach(e => print("%x".format(e)))
    println
  }

  println(input)
  printHex(md5(input))
  print(md5(input).length)


  println(Stream.from(1).filter(e => startWithFiveZeros(md5(puzzleInput + e))).head)


}

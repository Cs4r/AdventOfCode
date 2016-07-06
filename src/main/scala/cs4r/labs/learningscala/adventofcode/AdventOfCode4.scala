package cs4r.labs.learningscala.adventofcode

import java.security.MessageDigest

object AdventOfCode4 extends App {

  val puzzleInput: String = "iwrupvqb"

  def md5(s: String) = {
    val instance: MessageDigest = MessageDigest.getInstance("MD5")
    instance.digest(s.getBytes)
  }

  def startWithNZeros(n: Int)(input: Array[Byte]) = {
    arrayToString(input).take(n).forall(_ == '0')
  }

  def startWithFiveZeros(input: Array[Byte]) = {
    startWithNZeros(5)(input)
  }

  def startWithSixZeros(input: Array[Byte]) = {
    startWithNZeros(6)(input)
  }

  def arrayToString(input: Array[Byte]): String = {
    input.map("%02x".format(_)).reduce(_ + _)
  }
  println("000001dbbfa3a5c83a2d506429c7b00e".take(5))
  println("000001dbbfa3a5c83a2d506429c7b00e".take(5).forall(_ == '0'))
  val fiveLeadingZeros = Stream.from(1).filter(e => startWithFiveZeros(md5(puzzleInput + e)))

  println(fiveLeadingZeros.head)

  val sixLeadingZeros = Stream.from(1).filter(e => startWithSixZeros(md5(puzzleInput + e)))

  println(sixLeadingZeros.head)
}

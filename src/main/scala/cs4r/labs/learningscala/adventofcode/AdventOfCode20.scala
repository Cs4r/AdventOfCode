package cs4r.labs.learningscala.adventofcode



object AdventOfCode20 extends App {

  val divisors : (Int => List[Int]) = n => {
     (1 to n/2).toList.filter( n  % _ == 0) ++ List(n)
  }

  val partA: Int => Int => Int = expected => n =>  {
    var i = n
    while(divisors(i).sum * 10 < expected) {i+=1}
    i
  }

  println(partA(33100000)(1))


  val partB: Int => Int => Int = expected => n =>  {
    var i = n
    while( divisors(i).dropWhile(_ * 50 < i).sum * 11 < expected) {i+=1}
    i
  }

  println(partB(33100000)(1))



}

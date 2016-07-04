package cs4r.labs.learningscala.adventofcode


object AdventOfCode2 extends App {

  val puzzleInput = "3x11x24\n13x5x19\n1x9x27\n24x8x21\n6x8x17\n19x18x22\n10x9x12\n12x2x5\n26x6x11\n9x23x15\n12x8x17\n13x29x10\n28x18x6\n22x28x26\n1x5x11\n29x26x12\n8x28x29\n27x4x21\n12x7x16\n7x4x23\n15x24x8\n15x14x2\n11x6x29\n28x19x9\n10x3x1\n5x20x13\n10x25x1\n22x17x7\n16x29x3\n18x22x8\n18x11x19\n21x24x20\n4x7x17\n22x27x12\n1x26x6\n5x27x24\n29x21x3\n25x30x2\n21x26x2\n10x24x27\n10x16x28\n18x16x23\n6x5x26\n19x12x20\n6x24x25\n11x20x7\n4x8x5\n2x13x11\n11x17x1\n13x24x6\n22x29x16\n4x24x20\n10x25x10\n12x29x23\n23x27x12\n11x21x9\n13x2x6\n15x30x2\n8x26x24\n24x7x30\n22x22x8\n29x27x8\n28x23x27\n13x16x14\n9x28x20\n21x4x30\n21x20x20\n11x17x30\n9x14x22\n20x2x6\n10x11x14\n1x8x23\n23x19x19\n26x10x13\n21x12x12\n25x7x24\n1x28x17\n20x23x9\n2x24x27\n20x24x29\n1x3x10\n5x20x14\n25x21x3\n15x5x22\n14x17x19\n27x3x18\n29x23x19\n14x21x19\n20x8x3\n22x27x12\n24x15x18\n9x10x19\n29x25x28\n14x22x6\n4x19x28\n4x24x14\n17x19x17\n7x19x29\n28x8x26\n7x20x16\n11x26x29\n2x18x3\n12x7x18\n11x15x21\n24x7x26\n2x22x23\n2x30x5\n1x19x8\n15x29x10\n15x26x22\n20x16x14\n25x29x22\n3x13x19\n1x12x30\n3x15x27\n19x9x11\n30x8x21\n26x12x20\n11x17x19\n17x25x1\n19x24x12\n30x6x20\n11x19x18\n18x15x29\n18x8x9\n25x15x5\n15x6x26\n13x27x19\n23x24x12\n3x15x28\n17x10x10\n15x4x7\n15x27x7\n21x8x11\n9x18x2\n7x20x20\n17x23x12\n2x19x1\n7x26x26\n13x23x8\n10x3x12\n11x1x9\n1x11x19\n25x14x26\n16x10x15\n7x6x11\n8x1x27\n20x28x17\n3x25x9\n30x7x5\n17x17x4\n23x25x27\n23x8x5\n13x11x1\n15x10x21\n22x16x1\n12x15x28\n27x18x26\n25x18x5\n21x3x27\n15x25x5\n29x27x19\n11x10x12\n22x16x21\n11x8x18\n6x10x23\n21x21x2\n13x27x28\n2x5x20\n23x16x20\n1x21x7\n22x2x13\n11x10x4\n7x3x4\n19x2x5\n21x11x1\n7x27x26\n12x4x23\n12x3x15\n25x7x4\n20x7x15\n16x5x11\n1x18x26\n11x27x10\n17x6x24\n19x13x16\n6x3x11\n4x19x18\n16x15x15\n1x11x17\n19x11x29\n18x19x1\n1x25x7\n8x22x14\n15x6x19\n5x30x18\n30x24x22\n11x16x2\n21x29x19\n20x29x11\n27x1x18\n20x5x30\n12x4x28\n3x9x30\n26x20x15\n18x25x18\n20x28x28\n21x5x3\n20x21x25\n19x27x22\n8x27x9\n1x5x15\n30x6x19\n16x5x15\n18x30x21\n4x15x8\n9x3x28\n18x15x27\n25x11x6\n17x22x15\n18x12x18\n14x30x30\n1x7x23\n27x21x12\n15x7x18\n16x17x24\n11x12x19\n18x15x21\n6x18x15\n2x21x4\n12x9x14\n19x7x25\n22x3x1\n29x19x7\n30x25x7\n6x27x27\n5x13x9\n21x4x18\n13x1x16\n11x21x25\n27x20x27\n14x25x9\n23x11x15\n22x10x26\n15x16x4\n14x16x21\n1x1x24\n17x27x3\n25x28x16\n12x2x29\n9x19x28\n12x7x17\n6x9x19\n15x14x24\n25x21x23\n26x27x25\n7x18x13\n15x10x6\n22x28x2\n15x2x14\n3x24x18\n30x22x7\n18x27x17\n29x18x7\n20x2x4\n4x20x26\n23x30x15\n5x7x3\n4x24x12\n24x30x20\n26x18x17\n6x28x3\n29x19x29\n14x10x4\n15x5x23\n12x25x4\n7x15x19\n26x21x19\n18x2x23\n19x20x3\n3x13x9\n29x21x24\n26x13x29\n30x27x4\n20x10x29\n21x18x8\n7x26x10\n29x16x21\n22x5x11\n17x15x2\n7x29x5\n6x18x15\n23x6x14\n10x30x14\n26x6x16\n24x13x25\n17x29x20\n4x27x19\n28x12x11\n23x20x3\n22x6x20\n29x9x19\n10x16x22\n30x26x4\n29x26x11\n2x11x15\n1x3x30\n30x30x29\n9x1x3\n30x13x16\n20x4x5\n23x28x11\n24x27x1\n4x25x10\n9x3x6\n14x4x15\n4x5x25\n27x14x13\n20x30x3\n28x15x25\n5x19x2\n10x24x29\n29x30x18\n30x1x25\n7x7x15\n1x13x16\n23x18x4\n1x28x8\n24x11x8\n22x26x19\n30x30x14\n2x4x13\n27x20x26\n16x20x17\n11x12x13\n28x2x17\n15x26x13\n29x15x25\n30x27x9\n2x6x25\n10x26x19\n16x8x23\n12x17x18\n26x14x22\n13x17x4\n27x27x29\n17x13x22\n9x8x3\n25x15x20\n14x13x16\n8x7x13\n12x4x21\n27x16x15\n6x14x5\n28x29x17\n23x17x25\n10x27x28\n1x28x21\n18x2x30\n25x30x16\n25x21x7\n2x3x4\n9x6x13\n19x6x10\n28x17x8\n13x24x28\n24x12x7\n5x19x5\n18x10x27\n16x1x6\n12x14x30\n1x2x28\n23x21x2\n13x3x23\n9x22x10\n10x17x2\n24x20x11\n30x6x14\n28x1x16\n24x20x1\n28x7x7\n1x24x21\n14x9x7\n22x8x15\n20x1x21\n6x3x7\n7x26x14\n5x7x28\n5x4x4\n15x7x28\n30x16x23\n7x26x2\n1x2x30\n24x28x20\n5x17x28\n4x15x20\n15x26x2\n1x3x23\n22x30x24\n9x20x16\n7x15x2\n6x21x18\n21x21x29\n29x10x10\n4x3x23\n23x2x18\n29x24x14\n29x29x16\n22x28x24\n21x18x24\n16x21x6\n3x9x22\n9x18x4\n22x9x9\n12x9x13\n18x21x14\n7x8x29\n28x28x14\n1x6x24\n11x11x3\n8x28x6\n11x16x10\n9x16x16\n6x6x19\n21x5x12\n15x17x12\n3x6x29\n19x1x26\n10x30x25\n24x26x21\n1x10x18\n6x1x16\n4x17x27\n17x11x27\n15x15x21\n14x23x1\n8x9x30\n22x22x25\n20x27x22\n12x7x9\n9x26x19\n26x25x12\n8x8x16\n28x15x10\n29x18x2\n25x22x6\n4x6x15\n12x18x4\n10x3x20\n17x28x17\n14x25x13\n14x10x3\n14x5x10\n7x7x22\n21x2x14\n1x21x5\n27x29x1\n6x20x4\n7x19x23\n28x19x27\n3x9x18\n13x17x17\n18x8x15\n26x23x17\n10x10x13\n11x5x21\n25x15x29\n6x23x24\n10x7x2\n19x10x30\n4x3x23\n22x12x6\n11x17x16\n6x8x12\n18x20x11\n6x2x2\n17x4x11\n20x23x22\n29x23x24\n25x11x21\n22x11x15\n29x3x9\n13x30x5\n17x10x12\n10x30x8\n21x16x17\n1x5x26\n22x15x16\n27x7x11\n16x8x18\n29x9x7\n25x4x17\n10x21x25\n2x19x21\n29x11x16\n18x26x21\n2x8x20\n17x29x27\n25x27x4\n14x3x14\n25x29x29\n26x18x11\n8x24x28\n7x30x24\n12x30x22\n29x20x6\n3x17x1\n6x15x14\n6x22x20\n13x26x26\n12x2x1\n7x14x12\n15x16x11\n3x21x4\n30x17x29\n9x18x27\n11x28x16\n22x3x25\n18x15x15\n2x30x12\n3x27x22\n10x8x8\n26x16x14\n15x2x29\n12x10x7\n21x20x15\n2x15x25\n4x14x13\n3x15x13\n29x8x3\n7x7x28\n15x10x24\n23x15x5\n5x7x14\n24x1x22\n1x11x13\n26x4x19\n19x16x26\n5x25x5\n17x25x14\n23x7x14\n24x6x17\n5x13x12\n20x20x5\n22x29x17\n11x17x29\n25x6x4\n29x8x16\n28x22x24\n24x23x17\n16x17x4\n17x8x25\n22x9x13\n24x4x8\n18x10x20\n21x23x21\n13x14x12\n23x26x4\n4x10x29\n2x18x18\n19x5x21\n2x27x23\n6x29x30\n21x9x20\n6x5x16\n25x10x27\n5x29x21\n24x14x19\n19x11x8\n2x28x6\n19x25x6\n27x1x11\n6x8x29\n18x25x30\n4x27x26\n8x12x1\n7x17x25\n7x14x27\n12x9x5\n14x29x13\n18x17x5\n23x1x3\n28x5x13\n3x2x26\n3x7x11\n1x8x7\n12x5x4\n2x30x21\n16x30x11\n3x26x4\n16x9x4\n11x9x22\n23x5x6\n13x20x3\n4x3x2\n14x10x29\n11x8x12\n26x15x16\n7x17x29\n18x19x18\n8x28x4\n22x6x13\n9x23x7\n11x23x20\n13x11x26\n15x30x13\n1x5x8\n5x10x24\n22x25x17\n27x20x25\n30x10x21\n16x28x24\n20x12x8\n17x25x1\n30x14x9\n14x18x6\n8x28x29\n12x18x29\n9x7x18\n6x12x25\n20x13x24\n22x3x12\n5x23x22\n8x10x17\n7x23x5\n10x26x27\n14x26x19\n10x18x24\n8x4x4\n16x15x11\n3x14x9\n18x5x30\n29x12x26\n16x13x12\n15x10x7\n18x5x26\n14x1x6\n10x8x29\n3x4x9\n19x4x23\n28x17x23\n30x7x17\n19x5x9\n26x29x28\n22x13x17\n28x2x1\n20x30x8\n15x13x21\n25x23x19\n27x23x1\n4x6x23\n29x29x24\n5x18x7\n4x6x30\n17x15x2\n27x4x2\n25x24x14\n28x8x30\n24x29x5\n14x30x14\n10x18x19\n15x26x22\n24x19x21\n29x23x27\n21x10x16\n7x4x29\n14x21x3\n21x4x28\n17x16x15\n24x7x13\n21x24x15\n25x11x16\n10x26x13\n23x20x14\n20x29x27\n14x24x14\n14x23x12\n18x6x5\n3x18x9\n8x18x19\n20x26x15\n16x14x13\n30x16x3\n17x13x4\n15x19x30\n20x3x8\n13x4x5\n12x10x15\n8x23x26\n16x8x15\n22x8x11\n12x11x18\n28x3x30\n15x8x4\n13x22x13\n21x26x21\n29x1x15\n28x9x5\n27x3x26\n22x19x30\n4x11x22\n21x27x20\n22x26x7\n19x28x20\n24x23x16\n26x12x9\n13x22x9\n5x6x23\n20x7x2\n18x26x30\n3x6x28\n24x18x13\n28x19x16\n25x21x25\n25x19x23\n22x29x10\n29x19x30\n4x7x27\n5x12x28\n8x26x6\n14x14x25\n17x17x2\n5x27x11\n8x2x2\n3x20x24\n26x10x9\n22x28x27\n18x15x20\n12x11x1\n5x14x30\n7x3x16\n2x16x16\n18x20x15\n13x14x29\n1x17x12\n13x5x23\n19x4x10\n25x19x11\n15x17x14\n1x28x27\n11x9x28\n9x10x18\n30x11x22\n21x21x20\n2x1x5\n2x25x1\n7x3x4\n22x15x29\n21x28x15\n12x12x4\n21x30x6\n15x10x7\n10x14x6\n21x26x18\n14x25x6\n9x7x11\n22x3x1\n1x16x27\n1x14x23\n2x13x8\n14x19x11\n21x26x1\n4x28x13\n12x16x20\n21x13x9\n3x4x13\n14x9x8\n21x21x12\n27x10x17\n6x20x6\n28x23x23\n2x28x12\n8x10x10\n3x9x2\n20x3x29\n19x4x16\n29x24x9\n26x20x8\n15x28x26\n18x17x10\n7x22x10\n20x15x9\n6x10x8\n7x26x21\n8x8x16\n15x6x29\n22x30x11\n18x25x8\n6x21x20\n7x23x25\n8x25x26\n11x25x27\n22x18x23\n3x2x14\n16x16x1\n15x13x11\n3x9x25\n29x25x24\n9x15x1\n12x4x1\n23x30x20\n3x1x23\n6x10x29\n28x13x24\n4x19x17\n6x6x25\n27x29x17\n12x13x2\n10x7x13\n14x15x8\n22x2x3\n27x17x19\n23x10x16\n5x9x25\n9x25x14\n11x18x6\n18x10x12\n9x4x15\n7x16x14\n17x24x10\n11x4x6\n12x9x17\n22x18x12\n6x24x24\n6x22x23\n5x17x30\n6x9x5\n17x20x10\n6x8x12\n14x17x13\n29x10x17\n22x4x5\n10x19x30\n22x29x11\n10x12x29\n21x22x26\n16x6x25\n1x26x24\n30x17x16\n27x28x5\n30x13x22\n7x26x12\n11x24x30\n1x17x25\n22x1x3\n29x24x6\n4x8x24\n13x9x20\n8x12x9\n21x25x4\n23x23x28\n5x2x19\n29x3x15\n22x1x14\n3x23x30\n8x25x3\n15x8x14\n30x14x6\n23x27x24\n19x1x2\n10x9x13\n13x8x7\n8x13x22\n5x15x20\n17x14x8\n5x11x20\n5x10x27\n24x17x19\n21x2x3\n15x30x26\n21x19x15\n2x7x23\n13x17x25\n30x15x19\n26x4x10\n2x25x8\n9x9x10\n2x25x8\n19x21x30\n17x26x12\n7x5x10\n2x22x14\n10x17x30\n1x8x5\n23x2x25\n22x29x8\n13x26x1\n26x3x30\n25x17x8\n25x18x26\n26x19x15\n8x28x10\n12x16x29\n30x6x29\n28x19x4\n27x26x18\n15x23x17\n5x21x30\n8x11x13\n2x26x7\n19x9x24\n3x22x23\n6x7x18\n4x26x30\n13x25x20\n17x3x15\n8x20x18\n23x18x23\n28x23x9\n16x3x4\n1x29x14\n20x26x22\n3x2x22\n23x8x17\n19x5x17\n21x18x20\n17x21x8\n30x28x1\n29x19x23\n12x12x11\n24x18x7\n21x18x14\n14x26x25\n9x11x3\n10x7x15\n27x6x28\n14x26x4\n28x4x1\n22x25x29\n6x26x6\n1x3x13\n26x22x12\n6x21x26\n23x4x27\n26x13x24\n5x24x28\n22x16x7\n3x27x24\n19x28x2\n11x13x9\n29x16x22\n30x10x24\n14x14x22\n22x23x16\n14x8x3\n20x5x14\n28x6x13\n3x15x25\n4x12x22\n15x12x25\n10x11x24\n7x7x6\n8x11x9\n21x10x29\n23x28x30\n8x29x26\n16x27x11\n1x10x2\n24x20x16\n7x12x28\n28x8x20\n14x10x30\n1x19x6\n4x12x20\n18x2x7\n24x18x17\n16x11x10\n1x12x22\n30x16x28\n18x12x11\n28x9x8\n23x6x17\n10x3x11\n5x12x8\n22x2x23\n9x19x14\n15x28x13\n27x20x23\n19x16x12\n19x30x15\n8x17x4\n10x22x18\n13x22x4\n3x12x19\n22x16x23\n11x8x19\n8x11x6\n7x14x7\n29x17x29\n21x8x12\n21x9x11\n20x1x27\n1x22x11\n5x28x4\n26x7x26\n30x12x18\n29x11x20\n3x12x15\n24x25x17\n14x6x11"

  def surfaceArea(l: Int, h: Int, w: Int): Int = {
    2 * l * w + 2 * w * h + 2 * h * l
  }

  def smallestArea(l: Int, h: Int, w: Int): Int = {
    math.min(math.min(l * h, l * w), h * w)
  }

  def smallestPerimeter(l: Int, h: Int, w: Int): Int = {
    math.min(math.min(l * 2 + h * 2, h * 2 + w * 2), l * 2 + w * 2)
  }

  def wrapping(l: Int, h: Int, w: Int) = {
    surfaceArea(l, h, w) + smallestArea(l, h, w)
  }

  def ribbon(l: Int, h: Int, w: Int) = {
    smallestPerimeter(l, h, w) + volume(l, h, w)
  }

  def volume(l: Int, h: Int, w: Int): Int = {
    l * h * w
  }

  val puzzleInputAsTriplet = puzzleInput.split("\n").toList.map(e => e.split("x") match {
    case Array(f1, f2, f3) => (f1.toInt, f2.toInt, f3.toInt)
  })

  val wrappingPaperToOrder = puzzleInputAsTriplet.foldLeft(0)((s, x) => s + wrapping(x._1, x._2, x._3))

  val ribbonToOrder = puzzleInputAsTriplet.map((ribbon _).tupled).reduce(_+_)

  println(wrappingPaperToOrder)

  println(ribbonToOrder)
}

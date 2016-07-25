package cs4r.labs.learningscala.adventofcode

object AdventOfCode15 extends App {

  val puzzleInput =
    """Sprinkles: capacity 2, durability 0, flavor -2, texture 0, calories 3
      |Butterscotch: capacity 0, durability 5, flavor -3, texture 0, calories 3
      |Chocolate: capacity 0, durability 0, flavor 5, texture -1, calories 8
      |Candy: capacity 0, durability -1, flavor 0, texture 5, calories 8""".stripMargin

  val linePattern = "(\\w+):\\s.*\\s(-?\\p{Digit}+).*\\s(-?\\p{Digit}+).*\\s(-?\\p{Digit}+).*\\s(-?\\p{Digit}+).*\\s(-?\\p{Digit}+).*".r

  val listOfIngredients = puzzleInput.split("\r\n").map(l => {
    val linePattern(name, capacity, durability, flavor, texture, calories) = l
    IngredientInfo(name, capacity.toInt, durability.toInt, flavor.toInt, texture.toInt, calories.toInt)
  }).toList

  val numberOfIngredients = listOfIngredients.size

  case class IngredientInfo(val name: String, val capacity: Int, val durability: Int, val flavor: Int, val texture: Int, val calories: Int)

  case class Cookie(ingredients: List[IngredientInfo], teaspoonsOfEachIngredient: List[Int]) {
    private val teaspoonsPerIngredient = ingredients.zip(teaspoonsOfEachIngredient)

    val cookieCapacity = {
      val result = teaspoonsPerIngredient.map(ingredientAndTimes => ingredientPropertyScore(ingredientAndTimes._1.capacity, ingredientAndTimes._2)).sum
      zeroIfNegative(result)
    }

    val cookieDurability = {
      val result = teaspoonsPerIngredient.map(ingredientAndTimes => ingredientPropertyScore(ingredientAndTimes._1.durability, ingredientAndTimes._2)).sum
      zeroIfNegative(result)
    }

    val cookieFlavor = {
      val result = teaspoonsPerIngredient.map(ingredientAndTimes => ingredientPropertyScore(ingredientAndTimes._1.flavor, ingredientAndTimes._2)).sum
      zeroIfNegative(result)
    }

    val cookieTexture = {
      val result = teaspoonsPerIngredient.map(ingredientAndTimes => ingredientPropertyScore(ingredientAndTimes._1.texture, ingredientAndTimes._2)).sum
      zeroIfNegative(result)
    }

    val cookieCalories = {
      val result = teaspoonsPerIngredient.map(ingredientAndTimes => ingredientPropertyScore(ingredientAndTimes._1.calories, ingredientAndTimes._2)).sum
      zeroIfNegative(result)
    }

    private def ingredientPropertyScore(property: Int, times: Int): Int = {
      property * times
    }

    private def zeroIfNegative(result: Int): Int = if (result < 0) 0 else result


    def score: Int = {
      cookieCapacity * cookieDurability * cookieFlavor * cookieTexture
    }

  }

  val combinations = (List.fill(numberOfIngredients)(0 to 100).flatten).combinations(numberOfIngredients).filter(_.sum == 100).map(_.permutations).flatten.toList

  val partAResult = combinations.map(c => {
    Cookie(listOfIngredients, c).score
  }).max

  println(partAResult)

  val partBResult = combinations.map(c => {
    Cookie(listOfIngredients, c)
  }).filter(_.cookieCalories == 500).map(_.score).max


  println(partBResult)

}

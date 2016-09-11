package cs4r.labs.learningscala.adventofcode

/**
  * Created by cs4r on 5/09/16.
  */
object AdventOfCode19 extends App {


  val puzzleInput =
    """Al => ThF
      |Al => ThRnFAr
      |B => BCa
      |B => TiB
      |B => TiRnFAr
      |Ca => CaCa
      |Ca => PB
      |Ca => PRnFAr
      |Ca => SiRnFYFAr
      |Ca => SiRnMgAr
      |Ca => SiTh
      |F => CaF
      |F => PMg
      |F => SiAl
      |H => CRnAlAr
      |H => CRnFYFYFAr
      |H => CRnFYMgAr
      |H => CRnMgYFAr
      |H => HCa
      |H => NRnFYFAr
      |H => NRnMgAr
      |H => NTh
      |H => OB
      |H => ORnFAr
      |Mg => BF
      |Mg => TiMg
      |N => CRnFAr
      |N => HSi
      |O => CRnFYFAr
      |O => CRnMgAr
      |O => HP
      |O => NRnFAr
      |O => OTi
      |P => CaP
      |P => PTi
      |P => SiRnFAr
      |Si => CaSi
      |Th => ThCa
      |Ti => BP
      |Ti => TiTi
      |e => HF
      |e => NAl
      |e => OMg""".stripMargin

  val replacements = puzzleInput.split("\n").map(line => {
    val pieces = line.split("=>")
    val key = pieces(0).trim
    val value = pieces(1).trim
    (key, value)
  }).toList

  println(replacements)


  val molecule = "CRnSiRnCaPTiMgYCaPTiRnFArSiThFArCaSiThSiThPBCaCaSiRnSiRnTiTiMgArPBCaPMgYPTiRnFArFArCaSiRnBPMgArPRnCaPTiRnFArCaSiThCaCaFArPBCaCaPTiTiRnFArCaSiRnSiAlYSiThRnFArArCaSiRnBFArCaCaSiRnSiThCaCaCaFYCaPTiBCaSiThCaSiThPMgArSiRnCaPBFYCaCaFArCaCaCaCaSiThCaSiRnPRnFArPBSiThPRnFArSiRnMgArCaFYFArCaSiRnSiAlArTiTiTiTiTiTiTiRnPMgArPTiTiTiBSiRnSiAlArTiTiRnPMgArCaFYBPBPTiRnSiRnMgArSiThCaFArCaSiThFArPRnFArCaSiRnTiBSiThSiRnSiAlYCaFArPRnFArSiThCaFArCaCaSiThCaCaCaSiRnPRnCaFArFYPMgArCaPBCaPBSiRnFYPBCaFArCaSiAl"

  val calibrate: (String, String, String) => Set[String] = (str, toReplace, replacement) => {

    var toReturn =  Set[String]()

    var index = str.indexOf(toReplace)
    while (index >= 0) {
      toReturn +=  str.substring(0, index) + replacement + str.substring(index + toReplace.length, str.length)
      index = str.indexOf(toReplace, index + 1)
    }

    toReturn
  }


  val molecules = replacements.flatMap({ case (key, value) => calibrate(molecule, key, value) }).toSet

  val partA = molecules.size

  println(partA)


  type Mutation = (String)

  val mutate : Mutation => Set[Mutation] = (mutation) => {

    replacements.map( {case (key, value) => calibrate(mutation, key, value) }).toSet.flatten

  }

  def findMutations(molecule: String, mutations: Set[Mutation], steps: Int): Unit = {
    val expectedMolecule = mutations.find(_ == molecule)

    if (expectedMolecule.isDefined)
      println(steps + " :  " + expectedMolecule)
    else {
      println("Steps and size: " + steps + ", " + mutations.size)
      findMutations(molecule, mutations.flatMap(mutate), steps + 1)
    }
  }

  findMutations(molecule, mutate("e"), 0)

}

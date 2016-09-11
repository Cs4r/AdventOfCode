package cs4r.labs.learningscala.adventofcode

import scala.annotation.tailrec

object AdventOfCode23 extends App {

  val puzzleInput = """jio a, +16
                      |inc a
                      |inc a
                      |tpl a
                      |tpl a
                      |tpl a
                      |inc a
                      |inc a
                      |tpl a
                      |inc a
                      |inc a
                      |tpl a
                      |tpl a
                      |tpl a
                      |inc a
                      |jmp +23
                      |tpl a
                      |inc a
                      |inc a
                      |tpl a
                      |inc a
                      |inc a
                      |tpl a
                      |tpl a
                      |inc a
                      |inc a
                      |tpl a
                      |inc a
                      |tpl a
                      |inc a
                      |tpl a
                      |inc a
                      |inc a
                      |tpl a
                      |inc a
                      |tpl a
                      |tpl a
                      |inc a
                      |jio a, +8
                      |inc b
                      |jie a, +4
                      |tpl a
                      |inc a
                      |jmp +2
                      |hlf a
                      |jmp -7""".stripMargin

  trait Instruction
  case class Half(r: Char) extends Instruction
  case class Triple(r: Char) extends Instruction
  case class Inc(r: Char) extends Instruction
  case class Jump(offset: Int) extends Instruction
  case class JumpIfEven(r: Char, offset: Int) extends  Instruction
  case class JumpIfOne(r: Char, offset: Int) extends  Instruction


 val instructions = puzzleInput.split("\n").map(l => {
    val instruccionPattern = "([a-z]{3}) (.*)".r
    val instruccionPattern(instructionCode, operands) = l

    instructionCode match {
      case "hlf" => Half(operands.trim.charAt(0))
      case "tpl" => Triple(operands.trim.charAt(0))
      case "inc" => Inc(operands.trim.charAt(0))
      case "jmp" => Jump(operands.trim.toInt)
      case "jie" =>
        val ops = operands.trim.split(",").map(_.trim).take(2)
        JumpIfEven(ops(0).charAt(0), ops(1).toInt)
      case "jio" =>
        val ops = operands.trim.split(",").map(_.trim).take(2)
        JumpIfOne(ops(0).charAt(0), ops(1).toInt)
    }
  }).toList


  type Registers = Map[Char, Long]

  val compute : (List[Instruction], Registers) => Registers = (instructions, registers) => {


    @tailrec
    def executeInstruction(regs: Registers, instructionNumber: Int): Registers = {
      if (instructionNumber < 0 || instructionNumber >= instructions.size) regs
      else {

        val toExecute = instructions(instructionNumber)

        println(s"Instruction number: $instructionNumber instruction: $toExecute, registers: $regs")

        val (newRegisters, nextInstruction) = toExecute match {
          case Half(r) => (regs + (r -> (regs.getOrElse(r, 0L) / 2)), instructionNumber + 1)

          case Inc(r) => (regs + (r -> (regs.getOrElse(r, 0L) + 1)), instructionNumber + 1)

          case Triple(r) => (regs + (r -> (regs.getOrElse(r, 0L) * 3)), instructionNumber + 1)

          case Jump(offset) => (regs, instructionNumber + offset)

          case JumpIfEven(r, offset) => (regs, if (regs.get(r).get % 2 == 0) instructionNumber + offset else instructionNumber + 1)

          case JumpIfOne(r, offset) => (regs, if (regs.get(r).get == 1) instructionNumber + offset else instructionNumber + 1)

        }


        executeInstruction(newRegisters, nextInstruction)
      }


    }

    executeInstruction(registers, 0)
  }

  println(s"We've got ${instructions.size} instructions")

  val partA = compute(instructions, Map('a' -> 0, 'b' -> 0))

  println(partA)

  val partB = compute(instructions, Map('a' -> 1, 'b' -> 0))

  println(partB)

}

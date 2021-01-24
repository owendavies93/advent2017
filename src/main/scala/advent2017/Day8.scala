package advent2017

import scalaadventutils.Problem

object Day8 {

    type Machine = Map[String, Int]

    val parser = """(\w+) (inc|dec) (-?\d+) if (\w+) ([<>=!]+) (-?\d+)""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day8")
        println(part1(input))
        println(part2(input))
    }

    def part1(lines: List[String]) =
        run(lines, Map().withDefaultValue(0)).values.max

    def part2(lines: List[String]) =
        runWithIntermediates(lines, Map().withDefaultValue(0))
            .flatMap(_.values).max

    def run(lines: List[String], regs: Machine) =
        lines.foldLeft(regs)((acc, i) => runCommand(i, acc))

    def runWithIntermediates(lines: List[String], regs: Machine) =
        lines.scanLeft(regs)((acc, i) => runCommand(i, acc))

    private def runCommand(line: String, regs: Machine) = line match {
        case parser(reg, com, amount, conR, conOp, conA) => {
            val comp = stringToCompOp(conOp)
            if (comp(regs(conR), conA.toInt)) {
                val op = stringToNumOp(com)
                regs.updated(reg, op(regs(reg), amount.toInt))
            } else {
                regs
            }
        }
    }

    private def stringToCompOp(op: String): (Int, Int) => Boolean =
        op match {
            case ">"  => _ > _
            case "<"  => _ < _
            case "<=" => _ <= _
            case ">=" => _ >= _
            case "==" => _ == _
            case "!=" => _ != _
        }

    private def stringToNumOp(op: String): (Int, Int) => Int =
        op match {
            case "inc" => _ + _
            case "dec" => _ - _
        }
}

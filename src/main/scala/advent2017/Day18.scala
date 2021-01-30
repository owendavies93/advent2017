package advent2017

import scalaadventutils.Problem

import annotation.tailrec

object Day18 {

    type Machine = Map[String, Long]

    val numeric = """(-?\d+)""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day18").map(_.split(" ").toList)
        println(part1(input))
    }

    def part1(lines: List[List[String]]) =
        findFreq(lines, Map[String, Long]().withDefaultValue(0))

    def findFreq(lines: List[List[String]], init: Machine): Long = {
        @tailrec
        def find_(s: ProgramState): Long = {
            if (s.ptr < 0 || s.ptr >= lines.size) s.freq
            else find_(runFreq(lines(s.ptr), s))
        }

        find_(ProgramState(init, 0, 0))
    }

    private def runFreq(line: List[String], s: ProgramState) = {
        val com :: args = line
        val reg = args(0)

        com match {
            case "snd" => s.setFreq(s.get(reg)).next
            case "set" => args(1) match {
                case numeric(_) => s.update(reg, args(1).toLong).next
                case _ => s.update(reg, s.get(args(1))).next
            }
            case "rcv" => if (s.regs(reg) != 0) s.jumpTo(-1) else s.next
            case "jgz" =>
                if (s.regs(reg) > 0) s.jump(args(1).toInt) else s.next
            case _ => args(1) match {
                case numeric(_) =>
                    s.update(reg, strToOp(com)(s.regs(reg), args(1).toLong)).next
                case _ =>
                    s.update(reg, strToOp(com)(s.regs(reg), s.regs(args(1)))).next
            }
        }
    }

    private def strToOp(op: String): (Long, Long) => Long =
        op match {
            case "add" => _ + _
            case "mul" => _ * _
            case "mod" => _ % _
        }
}

case class ProgramState(regs: Day18.Machine, ptr: Int, freq: Long) {

    def next = copy(ptr = ptr + 1)

    def jump(value: Int) = copy(ptr = ptr + value)

    def jumpTo(value: Int) = copy(ptr = value)

    def get(reg: String) = regs(reg)

    def update(reg: String, value: Long) = copy(regs = regs.updated(reg, value))

    def setFreq(value: Long) = copy(freq = value)
}


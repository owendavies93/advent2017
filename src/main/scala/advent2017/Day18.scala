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
        run(lines, Map[String, Long]().withDefaultValue(0))

    def run(lines: List[List[String]], init: Machine): Long = {
        @tailrec
        def run_(regs: Machine, freq: Long, ptr: Int): Long = {
            if (ptr < 0 || ptr >= lines.size) freq
            else {
                val (r, f, p) = runCommand(lines(ptr), regs, freq, ptr)
                run_(r, f, p)
            }
        }

        run_(init, 0, 0)
    }

    private def runCommand
        ( line: List[String]
        , regs: Machine
        , freq: Long
        , ptr: Int
        ) = {

        val com :: args = line
        val reg = args(0)

        com match {
            case "snd" => (regs, regs(reg), ptr + 1)
            case "set" => args(1) match {
                case numeric(_) =>
                    (regs.updated(reg, args(1).toLong), freq, ptr + 1)
                case _ => (regs.updated(reg, regs(args(1))), freq, ptr + 1)
            }
            case "rcv" =>
                if (regs(reg) != 0) (regs, freq, -1)
                else (regs, freq, ptr + 1)
            case "jgz" =>
                if (regs(reg) > 0) (regs, freq, ptr + args(1).toInt)
                else (regs, freq, ptr + 1)
            case _ => args(1) match {
                case numeric(_) => (
                    regs.updated(reg, strToOp(com)(regs(reg), args(1).toLong)),
                    freq, ptr + 1
                )
                case _ => (
                    regs.updated(reg, strToOp(com)(regs(reg), regs(args(1)))),
                    freq, ptr + 1
                )
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

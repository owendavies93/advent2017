package advent2017

import scalaadventutils.Problem
import scalaadventutils.Factorise

import annotation.tailrec

object Day23 {

    type Machine = Map[String, Long]

    val numeric = """(-?\d+)""".r

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day23").map(_.split(" ").toList)
        println(part1(input))

        /*
            Part two uses a modified input file, which calls a prime checking
            function within scala, and has no-op commands filled in to ensure
            the jumps still work as expected.

            You could fix the jumps and remove the no-op code, but this is far
            more simple!
        */
        val input2 = Problem.parseInputToList("day23-2").map(_.split(" ").toList)
        println(part2(input2))
    }

    def part1(lines: List[List[String]]) =
        countMul(lines, Map[String, Long]().withDefaultValue(0))

    def part2(lines: List[List[String]]) =
        findH(lines, Map[String, Long]("a" -> 1L).withDefaultValue(0))

    def countMul(lines: List[List[String]], init: Machine): Long = {
        @tailrec
        def count_(s: ProgramState): Long = {
            if (s.ptr < 0 || s.ptr >= lines.size) s.freq
            else count_(run(lines(s.ptr), s))
        }
        count_(ProgramState(init, 0, 0))
    }

    def findH(lines: List[List[String]], init: Machine): Long = {
        @tailrec
        def count_(s: ProgramState, count: Int): Long = {
            if (s.ptr < 0 || s.ptr >= lines.size) s.get("h")
            else count_(run(lines(s.ptr), s), count + 1)
        }
        count_(ProgramState(init, 0, 0), 0)
    }

    private def run(line: List[String], s: ProgramState) = {
        val com :: args = line
        val reg = args(0)

        com match {
            case "set" => args(1) match {
                case numeric(_) => s.update(reg, args(1).toLong).next
                case _ => s.update(reg, s.get(args(1))).next
            }
            case "jnz" => reg match {
                case numeric(_) => if (reg != 0) s.jump(args(1).toInt) else s.next
                case _ => if (s.get(reg) != 0) s.jump(args(1).toInt) else s.next
            }
            case "sub" => args(1) match {
                case numeric(_) =>
                    s.update(reg, s.get(reg) - args(1).toLong).next
                case _ =>
                    s.update(reg, s.get(reg) - s.get(args(1))).next
            }
            case "mul" => args(1) match {
                case numeric(_) =>
                    s.update(reg, s.get(reg) * args(1).toLong).setFreq(s.freq + 1).next
                case _ =>
                    s.update(reg, s.get(reg) * s.get(args(1))).setFreq(s.freq + 1).next
            }
            case "pri" =>
                if (Factorise.isPrime(s.get(args(1)).toInt)) s.update(reg, 1).next
                else s.update(reg, 0).next
            case "nop" => s.next
        }
    }
}

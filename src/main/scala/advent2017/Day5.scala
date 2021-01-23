package advent2017

import scalaadventutils.Problem

import annotation.tailrec

object Day5 {

    def main(args: Array[String]) {
        println(part1(parseInput("day5")))
        println(part2(parseInput("day5")))
    }

    def part1(jumps: List[Int]): Int = {
        @tailrec
        def runJumps_(jumps: List[Int], i: Int, steps: Int): Int = {
            if (i < 0 || i >= jumps.size) steps
            else runJumps_(
                jumps.updated(i, jumps(i) + 1), i + jumps(i), steps + 1
            )
        }

        runJumps_(jumps, 0, 0)
    }

    def part2(jumps: List[Int]): Int = {
        @tailrec
        def runJumps_(jumps: List[Int], i: Int, steps: Int): Int = {
            if (i < 0 || i >= jumps.size) steps
            else {
                val j = jumps(i)
                val update = if (j >= 3) j - 1 else j + 1
                runJumps_(jumps.updated(i, update), i + j, steps + 1)
            }
        }

        runJumps_(jumps, 0, 0)
    }

    def parseInput(file: String) = Problem.parseInputToList(file).map(_.toInt)
}

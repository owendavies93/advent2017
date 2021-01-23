package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day5Spec extends AnyFunSuite {

    val input = Day5.parseInput("day5-test")

    test("Day 5: part 1") {
        assertResult(5) {
            Day5.part1(input)
        }
    }

    test("Day 5: part 2") {
        assertResult(10) {
            Day5.part2(input)
        }
    }
}

package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day12Spec extends AnyFunSuite {

    test("Day 12: part 1") {
        val input = Problem.parseInputToList("day12-test")

        assertResult(6) {
            Day12.part1(input, 0)
        }
    }

    test("Day 12: part 2") {
        val input = Problem.parseInputToList("day12-test")

        assertResult(2) {
            Day12.part2(input)
        }
    }
}

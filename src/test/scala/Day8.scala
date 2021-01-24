package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day8Spec extends AnyFunSuite {

    test("Day 8: part 1") {
        val input = Problem.parseInputToList("day8-test")

        assertResult(1) {
            Day8.part1(input)
        }
    }

    test("Day 8: part 2") {
        val input = Problem.parseInputToList("day8-test")

        assertResult(10) {
            Day8.part2(input)
        }
    }
}

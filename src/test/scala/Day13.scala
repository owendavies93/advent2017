package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day13Spec extends AnyFunSuite {

    test("Day 13: part1") {
        val input = Problem.parseInputToList("day13-test").map(Day13.parseLine)

        assertResult(24) {
            Day13.part1(input)
        }
    }

    test("Day 13: part2") {
        val input = Problem.parseInputToList("day13-test").map(Day13.parseLine)

        assertResult(10) {
            Day13.part2(input)
        }
    }
}

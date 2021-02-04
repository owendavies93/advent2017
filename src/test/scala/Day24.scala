package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day24Spec extends AnyFunSuite {

    test("Day 24: findMatchingComponents") {
        val lines = Problem.parseInputToList("day24-test")
        val head :: tail = Day24.parseInput(lines)

        assert(Day24.findMatchingComponents(head.sockets(0), tail).size == 1)
        assert(Day24.findMatchingComponents(head.sockets(1), tail).size == 2)
    }

    test("Day 24: findStrongestAndLongest") {
        val lines = Problem.parseInputToList("day24-test")
        val cs = Day24.parseInput(lines)

        assertResult((31, 19)) {
            Day24.findStrongestAndLongest(cs)
        }
    }
}

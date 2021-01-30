package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day18Spec extends AnyFunSuite {

    test("Day 18: run") {
        val input = Problem.parseInputToList("day18-test").map(_.split(" ").toList)

        val init = Map("a" -> 0)

        assertResult(4) {
            Day18.run(input, init)
        }
    }
}

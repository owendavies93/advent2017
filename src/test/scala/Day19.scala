package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day19Spec extends AnyFunSuite {

    import advent2017.Direction._

    test("Day 19: traverse") {
        val lines = Problem.parseInputToList("day19-test")

        val grid = Day19.generateDiag(lines)

        assertResult(("ABCDEF", 38)) {
            Day19.traverse(grid, Point(5, 0), D)
        }
    }
}

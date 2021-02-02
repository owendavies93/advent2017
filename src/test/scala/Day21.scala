package advent2017

import scalaadventutils.GridUtils
import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day21Spec extends AnyFunSuite {

    test("Day 21: enhance") {
        val lines = Problem.parseInputToList("day21-test")

        val book = Day21.buildBook(lines)

        val g = List[String](
            ".#.",
            "..#",
            "###"
        )

        val grid = GridUtils.from2DCharArray(g, '#')

        assert(Day21.enhance(grid, book).countOn == 4)
    }

    test("Day 21: part1") {
        val lines = Problem.parseInputToList("day21-test")

        assertResult(12) {
            Day21.part1(2, lines)
        }
    }
}

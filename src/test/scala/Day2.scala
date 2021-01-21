package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day2Spec extends AnyFunSuite {

    test("Day 2: part 1") {
        val input = List(
            Array(5, 1, 9, 5),
            Array(7, 5, 3),
            Array(2, 4, 6, 8)
        )

        assertResult(18) {
            Day2.part1(input)
        }
    }

    test("Day 2: part 2") {
        val input = List(
            Array(5, 9, 2, 8),
            Array(9, 4, 7, 3),
            Array(3, 8, 6, 5)
        )

        assertResult(9) {
            Day2.part2(input)
        }
    }
}

package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day6Spec extends AnyFunSuite {

    test("Day 6: redistribute") {
        var bank = List(0, 2, 7, 0)
        bank = Day6.redistribute(bank)

        assertResult(List(2, 4, 1, 2)) {
            bank
        }

        bank = Day6.redistribute(bank)

        assertResult(List(3, 1, 2, 3)) {
            bank
        }

        bank = Day6.redistribute(bank)

        assertResult(List(0, 2, 3, 4)) {
            bank
        }
    }

    test("Day 6: part 1") {
        val bank = List(0, 2, 7, 0)

        assertResult(5) {
            Day6.part1(bank)
        }
    }

    test("Day 6: part 2") {
        val bank = List(0, 2, 7, 0)

        assertResult(4) {
            Day6.part2(bank)
        }
    }
}

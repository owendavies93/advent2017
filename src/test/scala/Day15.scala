package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day15Spec extends AnyFunSuite {

    test("Day 15: part1") {
        assertResult(1) {
            Day15.part1(5, 65, 8921)
        }

        assertResult(588) {
            Day15.part1(40000000, 65, 8921)
        }
    }

    test("Day 16: part2") {
        assertResult(0) {
            Day15.part2(5, 65, 8921)
        }

        assertResult(1) {
            Day15.part2(1056, 65, 8921)
        }

        assertResult(309) {
            Day15.part2(5000000, 65, 8921)
        }
    }
}

package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day1Spec extends AnyFunSuite {

    test("Day 1: part 1") {
        assertResult(3) {
            Day1.part1("1122")
        }

        assertResult(4) {
            Day1.part1("1111")
        }

        assertResult(0) {
            Day1.part1("1234")
        }

        assertResult(9) {
            Day1.part1("91212129")
        }
    }

    test("Day 1: part 2") {
        assertResult(6) {
            Day1.part2("1212")
        }

        assertResult(0) {
            Day1.part2("1221")
        }

        assertResult(4) {
            Day1.part2("123425")
        }

        assertResult(12) {
            Day1.part2("123123")
        }

        assertResult(4) {
            Day1.part2("12131415")
        }
    }
}

package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day4Spec extends AnyFunSuite {

    test("Day 4: part 1") {
        var a = List("aa bb cc dd ee")

        assertResult(1) {
            Day4.part1(a)
        }

        a = List("aa bb cc dd aa")

        assertResult(0) {
            Day4.part1(a)
        }

        a = List("aa bb cc dd aaa")

        assertResult(1) {
            Day4.part1(a)
        }
    }

    test("Day 4: part 2") {
        var a = List("abcde fghij")

        assertResult(1) {
            Day4.part2(a)

        }

        a = List("abcde xyz ecdab")

        assertResult(0) {
            Day4.part2(a)
        }

        a = List("a ab abc abd abf abj")

        assertResult(1) {
            Day4.part2(a)
        }

        a = List("iiii oiii ooii oooi oooo")

        assertResult(1) {
            Day4.part2(a)
        }

        a = List("oiii ioii iioi iiio")

        assertResult(0) {
            Day4.part2(a)
        }
    }
}

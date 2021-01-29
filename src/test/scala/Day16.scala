package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day16Spec extends AnyFunSuite {

    test("Day 16: part 1") {
        var comms = List("s1", "x3/4", "pe/b")
        val input = "abcde"

        assertResult("baedc") {
            Day16.part1(comms, input.size - 1, input)
        }

        comms = List("s3")

        assertResult("cdeab") {
            Day16.part1(comms, input.size - 1, input)
        }
    }
}

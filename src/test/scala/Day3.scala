package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day3Spec extends AnyFunSuite {

    test("Day 3: getPosition") {

        assertResult((0, 0)) {
            Day3.getPosition(1)
        }

        assertResult((2, 1)) {
            Day3.getPosition(12)
        }

        assertResult((0, -2)) {
            Day3.getPosition(23)
        }
    }

    test("Day 3: getSteps") {
        assertResult(0) {
            Day3.getSteps(1)
        }

        assertResult(3) {
            Day3.getSteps(12)
        }

        assertResult(2) {
            Day3.getSteps(23)
        }

        assertResult(31) {
            Day3.getSteps(1024)
        }
    }

    test("Day 3: sumNeighbours") {
        var spiral = Map((0, 0) -> 1).withDefaultValue(0)

        assertResult(1) {
            Day3.sumNeighbours(spiral, (0, 1))
        }

        spiral = Map((0, 0) -> 1, (0, 1) -> 1, (1, 1) -> 2).withDefaultValue(0)

        assertResult(4) {
            Day3.sumNeighbours(spiral, (1, 0))
        }
    }
}

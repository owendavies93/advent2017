package advent2017

import org.scalatest.funsuite.AnyFunSuite

class Day17Spec extends AnyFunSuite {

    test("Day 17: spinLock") {
        assertResult(638) {
            Day17.spinLock(3)
        }
    }
}

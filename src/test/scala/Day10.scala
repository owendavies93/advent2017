package advent2017

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.Queue

class Day10Spec extends AnyFunSuite {

    test("Day 10: part1") {
        val lengths = List(3, 4, 1, 5)

        assertResult(Queue(3, 4, 2, 1, 0)) {
            Day10.runHashes(0, 4, lengths).queue
        }

        assertResult(12) {
            Day10.part1(0, 4, lengths)
        }
    }
}

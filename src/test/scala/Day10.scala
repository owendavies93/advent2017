package advent2017

import scalaadventutils.CircularList

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.Queue

class Day10Spec extends AnyFunSuite {

    test("Day 10: part1") {
        val lengths = List(3, 4, 1, 5)
        val cl = CircularList(5)(0, 1, 2, 3, 4)

        assertResult(Queue(3, 4, 2, 1, 0)) {
            Day10.runHashes(cl, 0, 0, lengths)._1.queue
        }

        assertResult(12) {
            Day10.part1(0, 4, lengths)
        }
    }

    test("Day 10: getKnotHash") {
        var lengths = "test"

        assertResult("309ae6e8f3cb4acbfcdd3c3f30fcf3") {
            Day10.getKnotHash(0, 255, lengths)
        }
    }
}

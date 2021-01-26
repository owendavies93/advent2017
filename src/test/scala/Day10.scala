package advent2017

import scalaadventutils.CircularList

import org.scalatest.funsuite.AnyFunSuite
import scala.collection.mutable.Queue

class Day10Spec extends AnyFunSuite {

    test("Day 10: part1") {
        val lengths = List(3, 4, 1, 5)
        val cl = CircularList(5)(0, 1, 2, 3, 4)

        assertResult(12) {
            Day10.part1(0, 4, lengths)
        }
    }

    test("Day 10: getKnotHash") {
        var lengths = "AoC 2017"

        assertResult("33efeb34ea91902bb2f59c9920caa6cd") {
            KnotHash.getKnotHash(lengths)
        }
    }
}

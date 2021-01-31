package advent2017

import scalaadventutils.Problem

import org.scalatest.funsuite.AnyFunSuite

class Day20Spec extends AnyFunSuite {

    test("Day 20: part1") {
        val lines = Problem.parseInputToList("day20-test")

        Day20.part1(lines)
    }

    test("Day 20: part2") {
        val lines = Problem.parseInputToList("day20-test2")

        Day20.part2(lines)
    }
}

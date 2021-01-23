package advent2017

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import org.scalatest.funsuite.AnyFunSuite

class Day7Spec extends AnyFunSuite {

    val input = Problem.parseInputToList("day7-test")
    val g     = Day7.constructGraph(input)

    test("Day 7: parseLine") {
        var line = "pbga (66)"

        assertResult(("pbga", Map[String, Int]())) {
            Day7.parseLine(line)
        }

        line = "fwft (72) -> ktlj, cntj, xhth"

        assertResult(("fwft", Map("ktlj" -> 1, "cntj" -> 1, "xhth" -> 1))) {
            Day7.parseLine(line)
        }
    }

    test("Day 7: constructGraph") {
        assertResult(Map[String, Int]()) {
            g("pbga")
        }

        assertResult(Map("ktlj" -> 1, "cntj" -> 1, "xhth" -> 1)) {
            g("fwft")
        }
    }

    test("Day 7: part1") {
        assertResult("tknk") {
            Day7.part1(input)
        }
    }

    test("Day 7: getWeights") {
        assertResult(66) {
            Day7.getWeights(input)("pbga")
        }
    }

    test("Day 7: getTotalTowerWeight") {
        val weights = Day7.getWeights(input)
        val graph   = new WeightedUndirectedGraph(g)

        assertResult(251) {
            Day7.getTotalTowerWeight("ugml", graph, weights)
        }
    }

    test("Day 7: part2") {
        assertResult(60) {
            Day7.part2(input)
        }
    }
}

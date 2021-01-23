package advent2017

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

import annotation.tailrec

object Day7 {

    type Graph = Map[String, Map[String, Int]]

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day7")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]) =
        new WeightedUndirectedGraph(constructGraph(input)).getRootNodes.head

    def part2(input: List[String]) = {
        val weights = getWeights(input)
        val graph   = new WeightedUndirectedGraph(constructGraph(input))

        val root = graph.getRootNodes.head

        @tailrec
        def findWeightDifference(root: String): Int = {
            val ws = graph.neighbours(root).toList.map(n =>
                (n, getTotalTowerWeight(n, graph, weights))
            )

            val (diffN, diffW) = ws.filter {
                case (n, w) => ws.map(_._2).count(_ == w) == 1
            }.head

            val different_ws = childrenWeights(diffN, graph, weights)

            if (different_ws.toSet.size == 1) {
                val otherWeight = ws.filter(_._2 != diffW).head._2
                weights(diffN) + (otherWeight - diffW)
            } else {
                findWeightDifference(diffN)
            }
        }

        findWeightDifference(root)
    }

    def getTotalTowerWeight
        ( root: String
        , g: WeightedUndirectedGraph[String]
        , weights: Map[String, Int]): Int =
        weights(root) + childrenWeights(root, g, weights).sum

    private def childrenWeights
        ( root: String
        , g: WeightedUndirectedGraph[String]
        , weights: Map[String, Int]): List[Int] =
        g.neighbours(root).toList.map(getTotalTowerWeight(_, g, weights))

    def getWeights(input: List[String]) =
        input.map(_.replace("(", "").replace(")", "").split(" ").take(2))
             .map { case Array(str, weight) => (str -> weight.toInt) }.toMap

    def constructGraph(input: List[String]): Graph =
        input.map(parseLine).toMap

    def parseLine(line: String) = line.split(" -> ") match {
        case Array(l, r) => (l.split(" ")(0), r.split(", ").map(s => (s -> 1)).toMap)
        case _           => (line.split(" ")(0), Map[String, Int]())
    }
}

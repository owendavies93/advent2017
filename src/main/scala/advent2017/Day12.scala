package advent2017

import scalaadventutils.Problem
import scalaadventutils.WeightedUndirectedGraph

object Day12 {

    type Graph = Map[Int, Map[Int, Int]]

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day12")
        println(part1(lines, 0))
        println(part2(lines))
    }

    def part1(lines: List[String], start: Int) =
        new WeightedUndirectedGraph(constructGraph(lines))
            .getAllPaths(start).flatten.distinct.size

    def part2(lines: List[String]) =
        new WeightedUndirectedGraph(constructGraph(lines))
            .getAllConnectedComponents.size

    def constructGraph(lines: List[String]): Graph =
        lines.map(parseLine).toMap

    private def parseLine(line: String) = {
        val sides = line.split(" <-> ")
        val edges = sides(1).split(", ")
        if (edges.size == 1 && edges(0) == sides(0))
            (sides(0).toInt, Map[Int, Int]())
        else
            (sides(0).toInt,
             edges.filterNot(_ == sides(0)).map((_.toInt -> 1)).toMap)
    }
}

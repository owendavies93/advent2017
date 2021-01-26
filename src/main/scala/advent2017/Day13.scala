package advent2017

import scalaadventutils.Problem

object Day13 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day13").map(parseLine)
        println(part1(input))
        println(part2(input))
    }

    def part1(layers: List[Layer]) =
        layers.filter(checkLayer(_, 0))
              .map(l => l.depth * l.range)
              .sum

    def part2(layers: List[Layer]) =
        Stream.from(0).dropWhile(i =>
            !layers.filter(checkLayer(_, i)).isEmpty
        )(0)

    private def checkLayer(l: Layer, delay: Int) =
        (l.depth + delay) % (2 * (l.range - 1)) == 0

    def parseLine(line: String) = {
        val s = line.split(": ").map(_.toInt)
        new Layer(s(0), s(1))
    }
}

class Layer(val depth: Int, val range: Int)


package advent2017

import scalaadventutils.Grid
import scalaadventutils.GridUtils
import scalaadventutils.Problem

object Day21 {

    type Book = Map[Grid, Grid]

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day21")
        println(part1(5, lines))
        println(part1(18, lines))
    }

    def part1(iterations: Int, lines: List[String]): Int = {
        val init = List[String](".#.", "..#", "###")
        val book = buildBook(lines)

        (0 until iterations).foldLeft(GridUtils.from2DCharArray(init, '#'))(
            (next, i) => {
                val g = enhance(next, book)
                println(g.countOn)
                g
            }
        ).countOn
    }

    def enhance(g: Grid, b: Book): Grid = {
        val segments = if (g.width % 2 == 0) g.split(2) else g.split(3)
        val transformed = segments.map(b(_)).toList
        GridUtils.join(transformed)
    }

    def buildBook(rules: List[String]) =
        rules.map(r => r.split(" => ").map(_.replace("/", "")))
             .map(r => (strToGrid(r(0)) -> strToGrid(r(1))))
             .flatMap {
                case (from, to) => from.transformations zip to.transformations
             }
             .toMap

    private def strToGrid(input: String) =
        GridUtils.from1DCharArray(input.toCharArray, '#')
}

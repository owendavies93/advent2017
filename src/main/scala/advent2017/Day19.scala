package advent2017

import scalaadventutils.Problem

import annotation.tailrec

object Day19 {

    import advent2017.Direction._

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day19")
        println(part1(lines))
    }

    def part1(lines: List[String]) = {
        val grid =  Day19.generateGrid(lines)
        traverse(grid, Point(13, 0), D)
    }

    def traverse(g: Grid, start: Point, dir: Direction.Value): (String, Int) = {
        @tailrec
        def traverse_
            ( p: Point
            , d: Direction.Value
            , out: String
            , count: Int)
            : (String, Int) = {

            g.get(p) match {
                case ' ' => (out, count)
                case '|' => traverse_(next(p, d), d, out, count + 1)
                case '-' => traverse_(next(p, d), d, out, count + 1)
                case '+' => {
                    val next = g.nonGridNeighbours(p)
                                .filter(g.get(_) != ' ')
                                .filter(nextDir(p, _) != inverse(d))(0)

                    traverse_(next, nextDir(p, next), out, count + 1)
                }
                case _ => traverse_(next(p, d), d, out + g.get(p), count + 1)
            }
        }

        traverse_(start, dir, "", 0)
    }

    def inverse(dir: Direction.Value) = dir match {
        case U => D
        case D => U
        case L => R
        case R => L
    }

    def next(p: Point, dir: Direction.Value) = dir match {
        case U => Point(p.x, p.y - 1)
        case D => Point(p.x, p.y + 1)
        case L => Point(p.x - 1, p.y)
        case R => Point(p.x + 1, p.y)
    }

    def nextDir(from: Point, to: Point): Direction.Value =
        if (from.x < to.x) R
        else if (from.x > to.x) L
        else if (from.y < to.y) D
        else U

    def generateGrid(lines: List[String]) =
        Grid(lines.toArray.flatten, lines(0).size, lines.size)

    case class Grid(grid: Array[Char], width: Int, height: Int) {

        private val nonGridNeighbourList = List[Point](
            Point(-1, 0), Point(0, -1), Point(1, 0), Point(0, 1)
        )

        def get(p: Point) = grid(p.y * width + p.x)

        def inBounds(p: Point) =
            (0 until width).contains(p.x) && (0 until height).contains(p.y)

        def nonGridNeighbours(p: Point) =
            nonGridNeighbourList.map(n => Point(p.x + n.x, p.y + n.y))
                                .filter(inBounds)
    }

    case class Point(x: Int, y: Int)
}


package advent2017

import scalaadventutils.Problem

object Day22 {

    import advent2017.Direction._
    import advent2017.NodeStatus._

    type InfiniteGrid = Map[Point, NodeStatus.Value]
    type Virus = (Point, Direction.Value)

    def main(args: Array[String]) {
        val lines = Problem.parseInputToList("day22")
        val grid: InfiniteGrid = constructGrid(lines)
        val start: Virus = (Point(lines.size / 2, lines(0).size / 2), U)
        println(part1(lines, grid, start, 10000))
        println(part2(lines, grid, start, 10000000))
    }

    def part1
        ( lines: List[String]
        , grid: InfiniteGrid
        , start: Virus
        , iterations: Int) =
        (0 until iterations).foldLeft((grid, start, 0))((next, i) => {
            val (g, v, c) = next
            burst(g, v, c)
        })._3

    def part2
        ( lines: List[String]
        , grid: InfiniteGrid
        , start: Virus
        , iterations: Int) =
        (0 until iterations).foldLeft((grid, start, 0))((next, i) => {
            val (g, v, c) = next
            burst2(g, v, c)
        })._3

    def burst(grid: InfiniteGrid, v: Virus, count: Int) = {
        val newDir = grid(v._1) match {
            case Infected => rotateRight(v._2)
            case Clean    => rotateLeft(v._2)
        }

        val newStatus = grid(v._1) match {
            case Clean    => Infected
            case Infected => Clean
        }

        val newCount = if (newStatus == Infected) count + 1 else count

        (grid.updated(v._1, newStatus), (v._1.next(newDir), newDir), newCount)
    }

    def burst2(grid: InfiniteGrid, v: Virus, count: Int) = {
        val newDir = grid(v._1) match {
            case Clean    => rotateLeft(v._2)
            case Weakened => v._2
            case Infected => rotateRight(v._2)
            case Flagged  => reverse(v._2)
        }

        val newStatus = grid(v._1) match {
            case Clean    => Weakened
            case Weakened => Infected
            case Infected => Flagged
            case Flagged  => Clean
        }

        val newCount = if (newStatus == Infected) count + 1 else count

        (grid.updated(v._1, newStatus), (v._1.next(newDir), newDir), newCount)
    }

    def constructGrid(lines: List[String]) =
        (0 until lines.size).flatMap(y =>
            (0 until lines(0).size).map(x =>
                Point(x, y) -> (if (lines(y)(x) == '#') Infected else Clean)
            )
        ).toMap.withDefaultValue(Clean)

    private def rotateLeft(d: Direction.Value) = d match {
        case U => L
        case D => R
        case L => D
        case R => U
    }

    private def rotateRight(d: Direction.Value) = d match {
        case U => R
        case D => L
        case L => U
        case R => D
    }

    private def reverse(d: Direction.Value) = d match {
        case U => D
        case D => U
        case L => R
        case R => L
    }

    case class Point(x: Int, y: Int) {
        def next(d: Direction.Value) = d match {
            case U => Point(x, y - 1)
            case D => Point(x, y + 1)
            case L => Point(x - 1, y)
            case R => Point(x + 1, y)
        }
    }
}

object NodeStatus extends Enumeration {
    val Clean, Infected, Weakened, Flagged = Value
}


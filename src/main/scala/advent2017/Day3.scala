package advent2017

import math._

object Day3 {

    import advent2017.Direction._

    type Point = (Int, Int)
    type Spiral = Map[(Int, Int), Int]

    val neighbourList = List(
        (-1, -1), (-1, 0), (0, -1), (1, -1),
        (1, 1), (-1, 1), (1, 0), (0, 1)
    )

    val transform = Map[Direction.Value, (Int, Int)](
        U -> (0 , 1),
        R -> (1 , 0),
        L -> (-1, 0),
        D -> (0 ,-1)
    )

    val target = 289326

    def main(args: Array[String]) {
        println(part1)
        println(part2)
    }

    def part1 = getSteps(target)

    def part2 = {
        val spiral = Map((0, 0) -> 1, (1, 0) -> 1).withDefaultValue(0)
        sumSpiral(spiral, (1, 1), U, 1, 0)
    }

    // This is the starting point of an Ulam Spiral, before
    // filtering for primes
    def getPosition(i: Int): Point = {
        val row    = ceil((sqrt(i) - 1) / 2).toInt
        var corner = pow(2 * row + 1, 2).toInt
        val side   = 2 * row

        // just check the four quarters of the square in turn
        if (i > corner - side) (row - (corner - i), -row)
        else {
            corner = corner - side
            if (i > corner - side) (-row, -row + (corner - i))
            else {
                corner = corner - side
                if (i > corner - side) (-row, -row + (corner - i))
                else (row, row - (corner - i - side))
            }
        }
    }

    def sumSpiral
        ( s: Spiral
        , p: Point
        , curDir: Direction.Value
        , layer: Int
        , count: Int)
        : Int = {

        val nextSum = sumNeighbours(s, p)

        if (nextSum > target) nextSum
        else {
            val nextSpiral = s.updated(p, nextSum)
            curDir match {
                case U if count < layer * 2 - 2 =>
                    sumSpiral(nextSpiral, nextFromDir(p, U), U, layer, count + 1)
                case U =>
                    sumSpiral(nextSpiral, nextFromDir(p, L), L, layer, 0)
                case L if count < layer * 2 - 1 =>
                    sumSpiral(nextSpiral, nextFromDir(p, L), L, layer, count + 1)
                case L =>
                    sumSpiral(nextSpiral, nextFromDir(p, D), D, layer, 0)
                case D if count < layer * 2 - 1 =>
                    sumSpiral(nextSpiral, nextFromDir(p, D), D, layer, count + 1)
                case D =>
                    sumSpiral(nextSpiral, nextFromDir(p, R), R, layer, 0)
                case R if count < layer * 2 =>
                    sumSpiral(nextSpiral, nextFromDir(p, R), R, layer, count + 1)
                case R =>
                    sumSpiral(nextSpiral, nextFromDir(p, U), U, layer + 1, 0)
            }
        }
    }

    def sumNeighbours(s: Spiral, p: Point) =
        neighbourList.map(n => {
            s((p._1 + n._1, p._2 + n._2))
        }).sum


    def nextFromDir(p: Point, d: Direction.Value) = {
        val t = transform(d)
        (p._1 + t._1, p._2 + t._2)
    }

    def getSteps(i: Int): Int = {
        val p = getPosition(i)
        abs(p._1) + abs(p._2)
    }
}

object Direction extends Enumeration {
    val U, D, L, R = Value
}


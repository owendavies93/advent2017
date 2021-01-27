package advent2017

import scalaadventutils.Grid
import scalaadventutils.GridUtils

object Day14 {

    type Point = (Int, Int)

    def main(args: Array[String]) {
        val grid = constructGrid("jxqlasbh")
        println(part1(grid))
        println(part2(grid))
    }

    def part1(g: Grid) = g.countOn

    def part2(g: Grid) = findGroups(g).size

    def findGroups(g: Grid): List[Set[Point]] = {
        val used = g.getOn.toSet

        def getAllInGroup(point: Point, unclaimed: Set[Point]): Set[Point] = {
            val neighbours = g.nonDiagNeighbours(point._1, point._2)
                              .filter(unclaimed.contains)
            Set(point) ++ neighbours.flatMap(
                getAllInGroup(_, unclaimed - point -- neighbours)
            )
        }

        used.foldLeft(List[Set[Point]]()) {
            case (groups, point) if !groups.exists(_.contains(point)) =>
                groups :+ getAllInGroup(point, used)
            case (groups, _) => groups
        }
    }

    def constructGrid(input: String) =
        GridUtils.from2DCharArray(
            (0 to 127).map(i => hexStringToBitString(
                KnotHash.getKnotHash(input + "-" + i.toString)
            )).toList, '1')

    def hexStringToBitString(input: String) =
        input.map(ch => Integer.parseInt(ch.toString, 16))
             .map("0000" + _.toBinaryString takeRight 4)
             .mkString

}

package advent2017

import scalaadventutils.Problem

import scala.math._

object Day11 {

    type Point = (Int, Int)

    val transform = Map[String, Point](
        "n"  -> (0  ,  10),
        "s"  -> (0  , -10),
        "sw" -> (-5 , -5),
        "ne" -> (5  ,  5),
        "se" -> (5  , -5),
        "nw" -> (-5 ,  5)
    )

    def main(args: Array[String]) {
        val input = Problem.parseInputLineToList("day11", ",")
        println(part1(input))
        println(part2(input))
    }

    def part1(dirs: List[String]) = stepsAway(getEndpoint(dirs))

    def part2(dirs: List[String]) = getAllPoints(dirs).map(stepsAway(_)).max

    def getEndpoint(dirs: List[String]) = getAllPoints(dirs).last

    def stepsAway(p: Point) = (abs(p._1) + abs(p._2)) / 10

    private def getAllPoints(dirs: List[String]) =
        dirs.scanLeft((0, 0))((acc, i) => {
            val t = transform(i)
            (acc._1 + t._1, acc._2 + t._2)
        })
}

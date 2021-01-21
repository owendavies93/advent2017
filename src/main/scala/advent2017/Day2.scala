package advent2017

import scalaadventutils.Problem

object Day2 {

    def main(args: Array[String]) {
        val lines = Problem.parseInputToListOfIntArray("day2", " ")
        println(part1(lines))
        println(part2(lines))
    }

    def part1(input: List[Array[Int]]) =
        input.map(arr => arr.max - arr.min).sum

    def part2(input: List[Array[Int]]) =
        input.map(arr => {
            val div =
                (arr.combinations(2) ++ arr.combinations(2).map(_.reverse))
                .filter(c => c(0) % c(1) == 0).toList.head
            div(0) / div(1)
        }).sum.toInt

}

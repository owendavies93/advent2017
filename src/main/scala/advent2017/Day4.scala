package advent2017

import scalaadventutils.Problem

object Day4 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToList("day4")
        println(part1(input))
        println(part2(input))
    }

    def part1(input: List[String]) = input.filter(l => {
        val words = l.split(" ")
        words.toSet.size == words.size
    }).size

    def part2(input: List[String]) = input.filter(l =>
        l.split(" ").map(_.sorted).combinations(2)
                    .filter(c => c(0) == c(1)).isEmpty
    ).size
}

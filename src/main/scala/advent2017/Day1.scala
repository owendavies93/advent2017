package advent2017

import scalaadventutils.Problem

object Day1 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToString("day1")
        println(part1(input))
        println(part2(input))
    }

    def part1(s: String) = (s.toCharArray :+ s.head).sliding(2)
        .filter(x => x(0) == x(1)).map(_(0).toString.toInt).sum

    def part2(s: String) = s.toCharArray.zipWithIndex.filter {
        case (ch, i) => ch == s((i + s.size / 2) % s.size)
    }.map(_._1.toString.toInt).sum

}

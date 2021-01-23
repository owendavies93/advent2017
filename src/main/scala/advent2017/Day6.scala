package advent2017

import scalaadventutils.Problem

import annotation.tailrec

object Day6 {

    def main(args: Array[String]) {
        val input =
            Problem.parseInputToString("day6").split(" ").map(_.toInt).toList
        println(part1(input))
        println(part2(input))
    }

    def part1(bank: List[Int]) = {
        @tailrec
        def redistributeAndCheck
            ( seen: Set[List[Int]]
            , next: List[Int]
            , steps: Int)
            : Int = {

            if (seen.contains(next)) steps
            else redistributeAndCheck(
                seen + next, redistribute(next), steps + 1)
        }

        redistributeAndCheck(Set[List[Int]](), bank, 0)
    }

    type Seen = Map[List[Int], Int]

    def part2(bank: List[Int]) = {
        @tailrec
        def redistributeAndCheckDiff
            ( seen: Seen
            , next: List[Int]
            , steps: Int)
            : Int = {

            val found = seen.getOrElse(next, -1)
            if (found >= 0) steps - found
            else redistributeAndCheckDiff(
                seen.updated(next, steps), redistribute(next), steps + 1)
        }

        redistributeAndCheckDiff(Map[List[Int], Int](), bank, 0)
    }

    def redistribute(a: List[Int]): List[Int] = {
        val (max, i) = a.zipWithIndex.maxBy(_._1)
        var zero = a.updated(i, 0).map(
            b =>  b + max / a.size
        )
        val remainder = max % a.size
        (i + 1 to i + remainder).foreach(i_ => {
            val index = i_ % zero.size
            zero = zero.updated(index, zero(index) + 1)
        })
        zero
    }
}

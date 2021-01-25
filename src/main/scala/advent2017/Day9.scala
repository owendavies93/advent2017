package advent2017

import scalaadventutils.Problem

import annotation.tailrec

object Day9 {

    def main(args: Array[String]) {
        val input = Problem.parseInputToString("day9")

        println(score(input))
        println(countGarbage(input))
    }

    def score(input: String): Int = {
        @tailrec
        def score_
            ( input: List[Char]
            , score: Int
            , groups: Int
            , garbage: Boolean)
            : Int =

            input match {
                case Nil                    => score
                case '<' :: tail            => score_(tail, score, groups, true)
                case '>' :: tail            => score_(tail, score, groups, false)
                case '!' :: _ :: tail       => score_(tail, score, groups, garbage)
                case _   :: tail if garbage => score_(tail, score, groups, garbage)
                case '{' :: tail            => score_(tail, score, groups + 1, garbage)
                case '}' :: tail            => score_(tail, score + groups, groups - 1, garbage)
                case _   :: tail            => score_(tail, score, groups, garbage)
            }

        score_(input.toList, 0, 0, false)
    }

    def countGarbage(input: String): Int = {
        @tailrec
        def countGarbage_
            ( input: List[Char]
            , count: Int
            , garbage: Boolean)
            : Int =

            input match {
                case Nil => count
                case '>' :: tail            => countGarbage_(tail, count, false)
                case '!' :: _ :: tail       => countGarbage_(tail, count, garbage)
                case _   :: tail if garbage => countGarbage_(tail, count + 1, garbage)
                case '<' :: tail            => countGarbage_(tail, count, true)
                case _   :: tail            => countGarbage_(tail, count, garbage)
            }

        countGarbage_(input.toList, 0, false)
    }
}

package advent2017

import scalaadventutils.CircularList
import scalaadventutils.Problem

object Day10 {
    def main(args: Array[String]) {
        val lengths = Problem.parseInputLineToList("day10", ",").map(_.toInt)
        val str = Problem.parseInputToString("day10")
        println(part1(0, 255, lengths))
        println(KnotHash.getKnotHash(str))
    }

    def part1(start: Int, end: Int, lengths: List[Int]) = {
        val init = Knot(
            CircularList(end + 1 - start)((start to end):_*), 0, 0)

        lengths.foldLeft(init)(_.hash(_)).product
    }
}

object KnotHash {
    def getKnotHash(lengths: String) = {
        val asciiLengths = lengths.map(_.toInt).toList ++
                           List(17, 31, 73, 47, 23)

        val init = Knot(
            CircularList(256)((0 to 255):_*), 0, 0)

        val sparse = (1 to 64).foldLeft(init)((acc, _) =>
            asciiLengths.foldLeft(acc)(_.hash(_))
        ).elems.queue.toList


        sparse.grouped(16).map(_.reduce((a, b) => a ^ b))
              .map(_.formatted("%02x")).mkString
    }
}

case class Knot(elems: CircularList[Int], position: Int, skip: Int) {
    def hash(length: Int) = Knot(
        elems.reverseSection(position, position + length),
        (position + length + skip) % elems.size,
        skip + 1
    )

    def product = elems.queue.take(2).product
}


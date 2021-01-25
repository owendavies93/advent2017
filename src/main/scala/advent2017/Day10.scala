package advent2017

import scalaadventutils.CircularList
import scalaadventutils.Problem

object Day10 {

    def main(args: Array[String]) {
        val lengths = Problem.parseInputLineToList("day10", ",").map(_.toInt)
        val str = Problem.parseInputToString("day10")
        println(part1(0, 255, lengths))
        println(getKnotHash(0, 255, str))
    }

    def part1(start: Int, end: Int, lengths: List[Int]) = {
        val init = CircularList(end + 1 - start)((start to end):_*)
        runHashes(init, 0, 0, lengths)._1.queue.take(2).product
    }

    def getKnotHash(start: Int, end: Int, lengths: String) = {
        val asciiLengths = lengths.map(_.toInt).toList ++
                           List(17, 31, 73, 47, 23)

        val sparse = (1 to 64).foldLeft(
            (CircularList(end + 1 - start)((start to end):_*), 0, 0)
        )(
            (acc, _) => {
                val (cl, pos, skip) = acc
                runHashes(cl, pos, skip % end, asciiLengths)
            }
        )._1.queue.toList

        sparse.grouped(16).map(_.reduce((a, b) => a ^ b))
              .map(_.toHexString).mkString
    }

    def runHashes
        ( l: CircularList[Int]
        , pos: Int
        , skip: Int
        , lengths: List[Int]) =

        (lengths zip (skip to skip + lengths.size)).foldLeft((l, pos, skip))(
            (acc, next) => {
                val (length, skip) = next
                val (cl, curPos, _) = acc
                (
                    hash(cl, curPos, length),
                    (curPos + length + skip) % l.size,
                    skip
                )
            }
        )

    def hash(cl: CircularList[Int], position: Int, length: Int) =
        cl.reverseSection(position, position + length)

}

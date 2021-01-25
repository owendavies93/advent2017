package advent2017

import scalaadventutils.CircularList
import scalaadventutils.Problem

object Day10 {

    def main(args: Array[String]) {
        val lengths = Problem.parseInputLineToList("day10", ",").map(_.toInt)
        println(part1(0, 255, lengths))
    }

    def part1(start: Int, end: Int, lengths: List[Int]) = {
        val out = runHashes(start, end, lengths)
        out.queue.take(2).product
    }

    def runHashes(start: Int, end: Int, lengths: List[Int]) =
        lengths.zipWithIndex.foldLeft(
            (CircularList(end + 1 - start)((start to end):_*), 0)
        )(
            (acc, next) => {
                val (length, skip) = next
                val (cl, curPos) = acc
                (hash(cl, curPos, length),
                 (curPos + length + skip) % (end + 1 - start))
            }
        )._1

    def hash(cl: CircularList[Int], position: Int, length: Int) =
        cl.reverseSection(position, position + length)

}
